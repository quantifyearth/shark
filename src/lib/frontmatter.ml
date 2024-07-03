open Sexplib.Conv

type path = Fpath.t

let path_of_sexp = function
  | Ppx_sexp_conv_lib.Sexp.Atom s -> Fpath.v s
  | List _ -> Fpath.v ""

let sexp_of_path v = Ppx_sexp_conv_lib.Sexp.Atom (Fpath.to_string v)

type t = {
  variables : (string * string list) list;
  inputs : (string * path) list;
}
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v variables inputs = { variables; inputs }
let empty = { variables = []; inputs = [] }

let yaml_to_string = function
  | `String s -> Some s
  | `Float f ->
      if Float.is_integer f then Some (int_of_float f |> string_of_int)
      else Some (string_of_float f)
  | _ -> None

let string_list_of_yaml = function
  | `A lst -> Some (List.filter_map yaml_to_string lst)
  | `String s -> Some [ s ]
  | _ -> None

let dict_of_yaml = function `O assoc -> assoc | _ -> []

let of_toplevel_yaml = function
  | `O assoc ->
      let vars =
        List.filter_map
          (fun (k, v) ->
            match string_list_of_yaml v with
            | Some l -> Some (k, l)
            | None -> None)
          assoc
      in
      let raw_inputs : (string * Yaml.value) list =
        match List.assoc_opt "inputs" assoc with
        | None -> []
        | Some v -> dict_of_yaml v
      in
      let inputs =
        List.filter_map
          (fun (k, rp) ->
            match rp with
            | `String p -> (
                match Fpath.of_string p with
                | Error _ -> None
                | Ok p -> Some (k, Fpath.normalize p))
            | _ -> None)
          raw_inputs
      in
      { variables = vars; inputs }
  | `Null -> empty
  | _ -> failwith "Malformed variables in markdown frontmatter"

let of_string s = String.trim s |> Yaml.of_string |> Result.map of_toplevel_yaml
let variables t = t.variables
let inputs t = List.map (fun (_, v) -> v) t.inputs
let input_map t = t.inputs

let default_container_path t =
  let default = "/root" in
  let path = match (List.assoc_opt "path" t.variables) with
  | None -> default
  | Some pl -> (
    match (List.nth_opt pl 0) with
    | None -> default
    | Some p -> p
  ) in
  match Fpath.of_string path with
  | Error (`Msg m) -> failwith m
  | Ok p -> p
