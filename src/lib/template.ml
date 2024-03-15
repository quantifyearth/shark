type t = { variables : (string * string list) list }

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

let rec combinations lists =
  match lists with
  | [] -> [ [] ]
  | hd :: tl ->
      let rest = combinations tl in
      List.concat (List.map (fun x -> List.map (fun y -> x :: y) rest) hd)

let variable_combinations t =
  let keys = List.map fst t.variables in
  let values = List.map snd t.variables in
  let combos = combinations values in
  List.map (List.combine keys) combos

let find_and_replace ~key ~value s =
  let regexp = Str.regexp ("%{" ^ key ^ "}") in
  Str.global_replace regexp value s

let variables_to_path s =
  String.concat "_" s |> String.split_on_char '/' |> String.concat "_"
  |> String.split_on_char '.' |> String.concat ""

let template ~file_path ~directory =
  let open Eio in
  let template = Path.load file_path in
  match Astring.String.cuts ~sep:"---" template with
  | [ ""; frontmatter; markdown ] ->
      let t =
        String.trim frontmatter |> Yaml.of_string_exn |> of_yaml
        |> variable_combinations
      in
      let md = String.trim markdown in
      let mds =
        List.map
          (fun variables ->
            List.fold_left
              (fun (values, acc) (key, value) ->
                (value :: values, find_and_replace ~key ~value acc))
              ([], md) variables)
          t
      in
      List.iter
        (fun (vs, txt) ->
          let path = variables_to_path vs in
          let name =
            Filename.basename (Path.native_exn file_path)
            |> Filename.chop_extension
          in
          let output_path = Path.(directory / (name ^ "_" ^ path ^ ".md")) in
          Path.save ~create:(`If_missing 0o644) output_path txt)
        mds
  | _ -> failwith "Malformed frontmatter/markdown file"
