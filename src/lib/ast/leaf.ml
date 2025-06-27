open Sexplib.Conv

type style = Command | Map [@@deriving sexp]

type t = {
  id : int;
  command : Command.t;
  style : style;
  inputs : Datafile.t list;
  outputs : Datafile.t list;
}
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v id command style inputs outputs = { id; command; style; inputs; outputs }
let command l = l.command
let inputs l = l.inputs
let outputs l = l.outputs
let command_style l = l.style
let id l = l.id

let to_string_for_inputs l (file_subs_map : (string * string list) list) :
    string list =
  let template = Command.to_string l.command in

  let rec loop (acc : string list) subs =
    match subs with
    | [] -> acc
    | (template_path, substitutions) :: tl ->
        let updated a =
          List.map
            (fun s ->
              let f = Result.get_ok (Fpath.of_string s) in
              let basename = Fpath.basename f in

              let src_regexp = Str.regexp (template_path ^ "\\*?") in
              let dst_regexp = Str.regexp "\\+" in

              let p1 = Str.global_replace src_regexp s a in
              match p1 == s with
              | true -> p1
              | false -> Str.global_replace dst_regexp basename p1)
            substitutions
        in
        let u = List.map updated acc |> List.concat in
        loop u tl
  in
  loop [ template ] file_subs_map
