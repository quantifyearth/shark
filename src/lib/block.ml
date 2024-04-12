open Astring
open Sexplib.Conv

type build_and_run = { hash : string option; alias : string; body : string }
[@@deriving sexp]

type publish = { body : string; output : [ `Directory of string ] }
[@@deriving sexp]

type t =
  [ `Build of build_and_run | `Run of build_and_run | `Publish of publish ]
[@@deriving sexp]

let build_or_run ?hash ~alias ~body kind =
  match kind with
  | `Run -> `Run { alias; body; hash }
  | `Build -> `Build { alias; body; hash }

let publish ?(output = `Directory "./_shark") body = `Publish { body; output }
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
      Some (`Build { hash; alias = env; body })
  | "shark-run" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      Some (`Run { hash; alias = env; body })
  | "shark-publish" :: rest ->
      let output =
        match rest with
        | [] | [ "" ] -> None
        | [ dir ] -> Some (`Directory dir)
        | _ -> failwith "Unknown publishing output"
      in
      Some (publish ?output body)
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
  | `Build { hash; alias; _ } -> (
      Fmt.str "shark-build:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | `Run { hash; alias; _ } -> (
      Fmt.str "shark-run:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | `Publish _ -> "shark-publish"

let body : t -> string = function
  | `Publish { body; _ } | `Run { body; _ } | `Build { body; _ } -> body

let alias = function
  | `Publish _ -> invalid_arg "Expected body or run"
  | `Run b | `Build b -> b.alias

let hash = function
  | `Publish _ -> invalid_arg "Expected body or run"
  | `Run b | `Build b -> b.hash

let kind = function
  | `Build _ -> `Build
  | `Run _ -> `Run
  | `Publish _ -> `Publish

let output = function
  | `Publish { output; _ } -> output
  | _ -> invalid_arg "Expected a publish block"

let with_hash (b : t) hash =
  match b with
  | `Build b -> `Build { b with hash = Some hash }
  | `Run b -> `Run { b with hash = Some hash }
  | `Publish b -> `Publish b

let command_list : t -> string list = function
  | `Publish { body; _ } | `Run { body; _ } | `Build { body; _ } ->
      let regex_newline = Str.regexp "\\\\\n"
      and regex_comment = Str.regexp "#.*$"
      and regex_whitespace = Str.regexp "[\t ]+" in
      Str.global_replace regex_newline "" body
      |> Str.global_replace regex_comment ""
      |> String.cuts ~sep:"\n" |> List.map String.trim
      |> List.map (Str.global_replace regex_whitespace " ")
      |> List.filter_map (fun l -> match l with "" -> None | x -> Some x)

let digest : t -> string = function
  | `Publish { body; _ } | `Run { body; _ } | `Build { body; _ } ->
      Digest.string body
