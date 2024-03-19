module Frontmatter = struct
  type t = { inputs : string list }

  let empty = { inputs = [] }
  let inputs f = f.inputs

  let of_string raw =
    match String.trim raw |> Yaml.of_string with
    | Result.Error _ -> None
    | Result.Ok frontmatter -> (
        match frontmatter with
        | `O d ->
            let inputs =
              match List.assoc_opt "inputs" d with
              | None -> []
              | Some x -> (
                  match x with
                  | `A l ->
                      List.filter_map
                        (fun (item : Yaml.value) : string option ->
                          match item with `String s -> Some s | _ -> None)
                        l
                  | _ -> [])
            in
            Some { inputs }
        | _ -> None)
end
