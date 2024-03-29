open Astring

type kind = [ `Build | `Run ]

let pp_kind ppf = function
  | `Build -> Fmt.pf ppf "build"
  | `Run -> Fmt.pf ppf "run"

type t = { kind : kind; hash : string option; alias : string; body : string }

let v ?hash ~alias ~body kind = { alias; body; kind; hash }

let pp ppf t =
  Fmt.record
    [
      Fmt.field "kind" (fun t -> t.kind) pp_kind;
      Fmt.field "hash" (fun t -> t.hash) Fmt.(option string);
      Fmt.field "alias" (fun t -> t.alias) Fmt.string;
      Fmt.field "body" (fun t -> t.body) Fmt.string;
    ]
    ppf t

let of_info_string ?(default = fun ~info:_ ~body:_ -> None) ~body s =
  match Astring.String.cuts ~sep:":" s with
  | "shark-build" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      Some { kind = `Build; hash; alias = env; body }
  | "shark-run" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      Some { kind = `Run; hash; alias = env; body }
  | _ -> default ~info:s ~body

let to_info_string t =
  match t.kind with
  | `Build -> (
      Fmt.str "shark-build:%s" t.alias
      ^ match t.hash with Some hash -> ":" ^ hash | None -> "")
  | `Run -> (
      Fmt.str "shark-run:%s" t.alias
      ^ match t.hash with Some hash -> ":" ^ hash | None -> "")

let body b = b.body
let alias b = b.alias
let hash b = b.hash
let kind b = b.kind
let with_hash b hash = { b with hash = Some hash }

let command_list b =
  let regex_newline = Str.regexp "\\\\\n"
  and regex_comment = Str.regexp "#.*$"
  and regex_whitespace = Str.regexp "[\t ]+" in
  Str.global_replace regex_newline "" b.body
  |> Str.global_replace regex_comment ""
  |> String.cuts ~sep:"\n" |> List.map String.trim
  |> List.map (Str.global_replace regex_whitespace " ")
  |> List.filter_map (fun l -> match l with "" -> None | x -> Some x)
