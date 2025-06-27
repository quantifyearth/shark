open Sexplib.Conv
open Astring
module DatafileSet = Set.Make (Datafile)

(* Raw block information defining the kind of block it is *)
module Raw = struct
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

  let body : t -> string = function
    | Publish { body; _ }
    | Run { body; _ }
    | Build { body; _ }
    | Import { body; _ } ->
        body

  let alias = function
    | Import { alias = Some alias; _ } | Run { alias; _ } | Build { alias; _ }
      ->
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
          | None -> Fmt.failwith "Invalid import statement '%s'" s
        in
        String.cuts ~sep:"\n" (String.trim body) |> List.map cut_import

  let digest : t -> string = function
    | Import { body; _ }
    | Publish { body; _ }
    | Run { body; _ }
    | Build { body; _ } ->
        Digest.string body

  let import_spec b =
    let open Obuilder_spec in
    let commands =
      imports b
      |> List.concat_map (fun (url, target_path) ->
             match Uri.scheme url with
             | None | Some "file" ->
                 let fpath =
                   match Fpath.of_string (Uri.path url) with
                   | Ok p -> p
                   | Error (`Msg msg) ->
                       Fmt.failwith "Failed to parse path %s: %s" (Uri.path url)
                         msg
                 in
                 let relpath =
                   Option.get (Fpath.relativize ~root:(Fpath.v "/") fpath)
                 in
                 [
                   copy
                     [ Fpath.to_string relpath ]
                     ~dst:(Fpath.to_string target_path);
                 ]
             | Some "http" | Some "https" -> (
                 let src_path = Uri.path url in
                 match String.cut ~rev:true ~sep:"." src_path with
                 | Some (_, "git") ->
                     [
                       shell [ "/bin/sh"; "-c" ];
                       run ~network:[ "host" ] "apk add --no-cache git";
                       run ~network:[ "host" ]
                         "mkdir -p /data && git clone %s %s" (Uri.to_string url)
                         (Fpath.to_string target_path);
                     ]
                 | _ ->
                     [
                       shell [ "/bin/sh"; "-c" ];
                       run ~network:[ "host" ] "apk add --no-cache curl";
                       run ~network:[ "host" ] "mkdir -p /data && curl -o %s %s"
                         (Fpath.to_string target_path)
                         (Uri.to_string url);
                     ])
             | Some scheme -> Fmt.failwith "Unsupported import scheme %s" scheme)
    in
    stage ~from:(`Image "alpine") commands
end

type t = {
  hash : string list ref;
  context : string;
  raw : Raw.t;
  commands : Leaf.t list;
}
[@@deriving sexp]

let v context raw commands = { hash = ref []; context; raw; commands }
let raw h = h.raw
let kind h = Raw.kind h.raw
let commands h = h.commands
let context h = h.context
let digest h = Raw.digest h.raw
let hash h = match !(h.hash) with [] -> None | hd :: _ -> Some hd
let hashes h = !(h.hash)
let update_hash h hash = h.hash := hash :: !(h.hash)

let io h =
  let all_inputs, all_outputs =
    List.fold_left
      (fun acc v ->
        let inputs, outputs = acc in
        ( DatafileSet.union inputs (DatafileSet.of_list (Leaf.inputs v)),
          DatafileSet.union outputs (DatafileSet.of_list (Leaf.outputs v)) ))
      (DatafileSet.empty, DatafileSet.empty)
      h.commands
  in
  ( DatafileSet.to_list (DatafileSet.diff all_inputs all_outputs),
    DatafileSet.to_list all_outputs )

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
