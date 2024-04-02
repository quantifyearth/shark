open Sexplib.Conv

type path = Fpath.t

let path_of_sexp = function
  | Ppx_sexp_conv_lib.Sexp.Atom s -> Fpath.v s
  | List _ -> Fpath.v ""

let sexp_of_path v = Ppx_sexp_conv_lib.Sexp.Atom (Fpath.to_string v)

type t = { variables : (string * string list) list; inputs : path list }
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v variables inputs = { variables; inputs }
let empty = { variables = []; inputs = [] }

let yaml_to_string = function
  | `String s -> s
  | `Float f ->
      if Float.is_integer f then int_of_float f |> string_of_int
      else string_of_float f
  | _ -> failwith "Can only convert numbers and strings"

let string_list_of_yaml = function
  | `A lst -> List.map yaml_to_string lst
  | `String s -> [ s ]
  | yml ->
      failwith ("Malformed variables in markdown: " ^ Fmt.str "%a" Yaml.pp yml)

let of_yaml = function
  | `O assoc ->
      let vars = List.map (fun (k, v) -> (k, string_list_of_yaml v)) assoc in
      let raw_inputs =
        match List.assoc_opt "inputs" vars with None -> [] | Some v -> v
      in
      let inputs =
        List.map
          (fun p ->
            match Fpath.of_string p with
            | Error e ->
                failwith
                  (Printf.sprintf "Malformed input path %s"
                     (match e with `Msg x -> x))
            | Ok p -> Fpath.normalize p)
          raw_inputs
      in
      { variables = vars; inputs }
  | `Null -> empty
  | _ -> failwith "Malformed variables in markdown frontmatter"

let of_string s = String.trim s |> Yaml.of_string |> Result.map of_yaml
let variables t = t.variables
let inputs t = t.inputs
