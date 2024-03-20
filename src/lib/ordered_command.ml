module DataFile = struct
  type t = { id : int; path : string }

  let create id path = { id; path }
  let id d = d.id
  let path d = d.path
  let compare a b = Int.compare a.id b.id
end

type t = {
  id : int;
  command : Command.t;
  inputs : DataFile.t list;
  outputs : DataFile.t list;
}

let command o = o.command
let inputs o = o.inputs
let outputs o = o.outputs
let id o = o.id

let order_command_list (metadata : Frontmatter.t) (commands : Command.t list) :
    t list =
  let input_map =
    List.mapi
      (fun i f -> (f, DataFile.create i f))
      (Frontmatter.inputs metadata)
  in
  let counter = ref (List.length input_map) in

  let rec loop commands datafile_map =
    match commands with
    | [] -> []
    | hd :: tl ->
        let file_args = Command.file_args hd in

        (* TODO: dedup *)
        let inputs =
          List.filter_map
            (fun path -> List.assoc_opt path datafile_map)
            file_args
        in

        let outputs =
          List.filter_map
            (fun path ->
              match List.assoc_opt path datafile_map with
              | None ->
                  let id = !counter in
                  counter := !counter + 1;
                  Some (DataFile.create id path)
              | Some _ -> None)
            file_args
        in

        let x = { id = !counter; command = hd; inputs; outputs } in
        counter := !counter + 1;
        x
        :: loop tl
             (List.concat
                [
                  datafile_map; List.map (fun o -> (DataFile.path o, o)) outputs;
                ])
  in
  loop commands input_map
