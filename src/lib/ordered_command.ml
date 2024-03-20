module DataFile = struct
  type t = { id : int; path : string }

  let create id path = { id; path }
  let id d = d.id
  let path d = d.path
  let compare a b = Int.compare a.id b.id
end

module Leaf = struct
  type t = {
    id : int;
    command : Command.t;
    inputs : DataFile.t list;
    outputs : DataFile.t list;
  }

  let create id command inputs outputs  = { id ; command ; inputs ; outputs }
  let command o = o.command
  let inputs o = o.inputs
  let outputs o = o.outputs
  let id o = o.id
end

module CommandGroup = struct
  type t = { name : string ; children : Leaf.t list }

  let create name children = {name ; children}
  let name g = g.name
  let children g = g.children
end

(* Not yet an actual AST, actually an ASL :) *)
type t = CommandGroup.t list

let order_command_list (metadata : Frontmatter.t) (command_groups : (string * (Command.t list)) list): t =
  let input_map =
    List.mapi
      (fun i f -> (f, DataFile.create i f))
      (Frontmatter.inputs metadata)
  in
  let counter = ref (List.length input_map) in


  let _, ordered = List.fold_left_map (fun input_map g ->
    let name, commands = g in
    let rec loop commands datafile_map =
      match commands with
      | [] -> datafile_map, []
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

          let x = Leaf.create !counter hd inputs outputs in
          counter := !counter + 1;
          let updated_map, rest = loop tl
          (List.concat
              [
                datafile_map; List.map (fun o -> (DataFile.path o, o)) outputs;
              ])
          in
          (updated_map, x :: rest)
    in
    let updated_map, commands = loop commands input_map in
    (updated_map, CommandGroup.create name commands)
  ) input_map command_groups in
  ordered
