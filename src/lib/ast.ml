open Astring

module DataFile = struct
  type t = { id : int; path : string; subpath : string option }

  let create ?(subpath = None) id path = { id; path; subpath }
  let id d = d.id

  let path d =
    match Filename.extension d.path = "" with
    | false -> d.path
    | true ->
        let p = d.path in
        let last_char = String.sub ~start:(String.length p) p in
        if String.Sub.to_string last_char = "/" then p else p ^ "/"

  let path_nc d = d.path
  let subpath d = d.subpath
  let compare a b = Int.compare a.id b.id

  let is_dir d =
    (* a little hacky, we probably need to do something in the sharkdown here *)
    Filename.extension d.path = ""
end

module Leaf = struct
  type t = {
    id : int;
    command : Command.t;
    inputs : DataFile.t list;
    outputs : DataFile.t list;
  }

  let create id command inputs outputs = { id; command; inputs; outputs }
  let command o = o.command
  let inputs o = o.inputs
  let outputs o = o.outputs
  let id o = o.id
end

module CommandGroup = struct
  type t = { name : string; children : Leaf.t list }

  let create name children = { name; children }
  let name g = g.name
  let children g = g.children
end

(* Not yet an actual AST, actually an ASL :) *)
type t = CommandGroup.t list

let to_list cg = cg

let find_matching_datafile datafile_map path =
  match List.assoc_opt path datafile_map with
  | Some p -> Some p
  | None ->
      (* No full match, but can we find a prefix dir *)
      List.fold_left
        (fun acc i ->
          match acc with
          | Some x -> Some x
          | None -> (
              let ipath, df = i in
              match DataFile.is_dir df with
              | false -> None
              | true -> (
                  match String.is_prefix ~affix:(DataFile.path df) path with
                  | true ->
                      Some
                        (DataFile.create
                           ~subpath:
                             (Some
                                (String.Sub.to_string
                                   (String.sub ~start:(String.length ipath) path)))
                           (DataFile.id df) (DataFile.path df))
                  | false -> None)))
        None datafile_map

let order_command_list metadata command_groups =
  let input_map =
    List.mapi
      (fun i f ->
        let df = DataFile.create i f in
        (f, df))
      (Frontmatter.inputs metadata)
  in
  let counter = ref (List.length input_map) in

  let _, ordered =
    List.fold_left_map
      (fun input_map g ->
        let name, commands = g in
        let rec loop commands datafile_map =
          match commands with
          | [] -> (datafile_map, [])
          | hd :: tl ->
              let file_args = Command.file_args hd in

              (* TODO: dedup *)
              let inputs =
                List.filter_map
                  (fun p -> find_matching_datafile datafile_map p)
                  file_args
              in

              let outputs =
                List.filter_map
                  (fun path ->
                    match find_matching_datafile datafile_map path with
                    | None ->
                        let id = !counter in
                        counter := !counter + 1;
                        Some (DataFile.create id path)
                    | Some _ -> None)
                  file_args
              in

              let x = Leaf.create !counter hd inputs outputs in
              counter := !counter + 1;
              let updated_map, rest =
                loop tl
                  (List.concat
                     [
                       datafile_map;
                       List.map (fun o -> (DataFile.path_nc o, o)) outputs;
                     ])
              in
              (updated_map, x :: rest)
        in
        let updated_map, commands = loop commands input_map in
        (updated_map, CommandGroup.create name commands))
      input_map command_groups
  in
  ordered
