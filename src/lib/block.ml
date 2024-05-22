open Astring
open Sexplib.Conv

type build_and_run = { hash : string option; alias : string; body : string }
[@@deriving sexp]

type publish = { body : string; output : [ `Directory of string ] }
[@@deriving sexp]

type import = { body : string; alias : string option; hash : string option }
[@@deriving sexp]

type t =
  | Build of build_and_run
  | Run of build_and_run
  | Publish of publish
  | Import of import
[@@deriving sexp]

let build_or_run ?hash ~alias ~body kind =
  match kind with
  | `Run -> Run { alias; body; hash }
  | `Build -> Build { alias; body; hash }

let publish ?(output = `Directory "./_shark") body = Publish { body; output }
let import ?hash ?alias body = Import { body; alias; hash }
let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)

let of_info_string ?(default = fun ~info:_ ~body:_ -> None) ~body s =
  match Astring.String.cuts ~sep:":" s with
  | "shark-build" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      Some (Build { hash; alias = env; body })
  | "shark-run" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      Some (Run { hash; alias = env; body })
  | "shark-publish" :: rest ->
      let output =
        match rest with
        | [] | [ "" ] -> None
        | [ dir ] -> Some (`Directory dir)
        | _ -> failwith "Unknown publishing output"
      in
      Some (publish ?output body)
  | "shark-import" :: rest ->
      let alias, hash =
        match rest with
        | [ env ] -> (Some env, None)
        | [ env; hash ] -> (Some env, Some hash)
        | _ -> (None, None)
      in
      Some (Import { body; alias; hash })
  | _ -> default ~info:s ~body

let of_code_block ?default cb =
  let body = Cmarkit.Block.Code_block.code cb in
  let body =
    List.map Cmarkit.Block_line.to_string body |> String.concat ~sep:"\n"
  in
  match Cmarkit.Block.Code_block.info_string cb with
  | None -> None
  | Some (info, _) -> of_info_string ?default ~body info

let to_info_string = function
  | Build { hash; alias; _ } -> (
      Fmt.str "shark-build:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | Run { hash; alias; _ } -> (
      Fmt.str "shark-run:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | Publish _ -> "shark-publish"
  | Import { hash; alias; _ } -> (
      Fmt.str "shark-run"
      ^ (match alias with Some alias -> ":" ^ alias | None -> "")
      ^ match hash with Some hash -> ":" ^ hash | None -> "")

let body : t -> string = function
  | Publish { body; _ }
  | Run { body; _ }
  | Build { body; _ }
  | Import { body; _ } ->
      body

let alias = function
  | Import { alias = Some alias; _ } | Run { alias; _ } | Build { alias; _ } ->
      alias
  | _ -> invalid_arg "No alias found for block"

let hash = function
  | Publish _ -> None
  | Run b -> b.hash
  | Build b -> b.hash
  | Import b -> b.hash

let kind = function
  | Build _ -> `Build
  | Run _ -> `Run
  | Publish _ -> `Publish
  | Import _ -> `Import

let output = function
  | Publish { output; _ } -> output
  | _ -> invalid_arg "Expected a publish block"

let with_hash (b : t) hash =
  match b with
  | Build b -> Build { b with hash = Some hash }
  | Run b -> Run { b with hash = Some hash }
  | Publish b -> Publish b
  | Import i -> Import i

let command_list : t -> string list = function
  | Import { body; _ }
  | Publish { body; _ }
  | Run { body; _ }
  | Build { body; _ } ->
      let regex_newline = Str.regexp "\\\\\n"
      and regex_comment = Str.regexp "#.*$"
      and regex_whitespace = Str.regexp "[\t ]+" in
      Str.global_replace regex_newline "" body
      |> Str.global_replace regex_comment ""
      |> String.cuts ~sep:"\n" |> List.map String.trim
      |> List.map (Str.global_replace regex_whitespace " ")
      |> List.filter_map (fun l -> match l with "" -> None | x -> Some x)

let imports = function
  | Build _ | Run _ | Publish _ -> invalid_arg "Expected an import block"
  | Import { body; _ } ->
      let cut_import s =
        match String.cut ~sep:" " s with
        | Some (url, path) -> (
            match Fpath.of_string path with
            | Ok path -> (Uri.of_string url, path)
            | Error (`Msg msg) ->
                Fmt.failwith "Error parsing path %s: %s" path msg)
        | None -> Fmt.failwith "Invalid import statement %s" s
      in
      let imports = String.cuts ~sep:"\n" body in
      List.map cut_import imports

let digest : t -> string = function
  | Import { body; _ }
  | Publish { body; _ }
  | Run { body; _ }
  | Build { body; _ } ->
      Digest.string body

let import_spec b =
  let open Obuilder_spec in
  (* TODO: Support multi-import statements *)
  let url, path = imports b |> List.hd in
  match Uri.scheme url with
  | None | Some "file" ->
      (* Choose better image, just need tools to import? *)
      stage ~from:(`Image "alpine")
        [
          run "mkdir -p %s" (Fpath.to_string (Fpath.parent path));
          copy [ Uri.path url ] ~dst:(Fpath.to_string path);
        ]
  | Some "http" | Some "https" -> (
      let src_path = Uri.path url in
      match String.cut ~rev:true ~sep:"." src_path with
      | Some (_, "git") ->
          (* Choose better image, just need tools to import? *)
          stage ~from:(`Image "alpine")
            [
              shell [ "/bin/sh"; "-c" ];
              run ~network:[ "host" ] "apk add --no-cache git";
              run ~network:[ "host" ] "mkdir -p /data && git clone %s %s"
                (Uri.to_string url) (Fpath.to_string path);
            ]
      | _ ->
          (* Choose better image, just need tools to import? *)
          stage ~from:(`Image "alpine")
            [
              shell [ "/bin/sh"; "-c" ];
              run ~network:[ "host" ] "apk add --no-cache curl";
              run ~network:[ "host" ] "mkdir -p /data && curl -O %s %s"
                (Fpath.to_string path) (Uri.to_string url);
            ])
  | Some scheme -> Fmt.failwith "Unsupported import scheme %s" scheme
