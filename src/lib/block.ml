open Astring
module Rb = Shark_ast.Ast.Block.Raw

type t = Rb.t

let of_info_string ?(default = fun ~info:_ ~body:_ -> None) ~body s =
  match Astring.String.cuts ~sep:":" s with
  | "shark-build" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      let build = Rb.build_or_run ?hash ~alias:env ~body `Build in
      Some build
  | "shark-run" :: rest ->
      let env, hash =
        match rest with
        | [ env ] -> (env, None)
        | [ env; hash ] -> (env, Some hash)
        | _ -> failwith "Malformed env and hash"
      in
      let run = Rb.build_or_run ?hash ~alias:env ~body `Run in
      Some run
  | "shark-publish" :: rest ->
      let output =
        match rest with
        | [] | [ "" ] -> None
        | [ dir ] -> Some (`Directory dir)
        | _ -> failwith "Unknown publishing output"
      in
      Some (Rb.publish ?output body)
  | "shark-import" :: rest ->
      let alias, hash =
        match rest with
        | [ env ] -> (Some env, None)
        | [ env; hash ] -> (Some env, Some hash)
        | _ -> (None, None)
      in
      Some (Rb.import ?hash ?alias body)
  | _ -> default ~info:s ~body

let of_code_block ?default cb =
  let body = Cmarkit.Block.Code_block.code cb in
  let body =
    List.map Cmarkit.Block_line.to_string body |> String.concat ~sep:"\n"
  in
  match Cmarkit.Block.Code_block.info_string cb with
  | None -> None
  | Some (info, _) -> of_info_string ?default ~body info

let to_info_string v =
  match Rb.kind v with
  | `Build -> (
      let alias = Rb.alias v in
      let hash = Rb.hash v in
      Fmt.str "shark-build:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | `Run -> (
      let alias = Rb.alias v in
      let hash = Rb.hash v in
      Fmt.str "shark-run:%s" alias
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
  | `Publish -> "shark-publish"
  | `Import -> (
      let alias = try Some (Rb.alias v) with Invalid_argument _ -> None in
      let hash = Rb.hash v in
      Fmt.str "shark-import"
      ^ (match alias with Some alias -> ":" ^ alias | None -> "")
      ^ match hash with Some hash -> ":" ^ hash | None -> "")
