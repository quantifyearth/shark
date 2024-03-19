type t = { variables : (string * string list) list }

let empty = { variables = [] }

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
      { variables = vars }
  | _ -> failwith "Malformed variables in markdown frontmatter"

let of_string s = String.trim s |> Yaml.of_string |> Result.map of_yaml

let inputs t =
  match List.assoc_opt "inputs" t.variables with None -> [] | Some v -> v
