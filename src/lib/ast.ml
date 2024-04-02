open Sexplib.Conv

module Leaf = struct
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

  let v id command style inputs outputs =
    { id; command; style; inputs; outputs }

  let command o = o.command
  let inputs o = o.inputs
  let outputs o = o.outputs
  let command_style o = o.style
  let id o = o.id
end

module CommandGroup = struct
  type t = { name : string; children : Leaf.t list } [@@deriving sexp]

  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
  let v name children = { name; children }
  let name g = g.name
  let children g = g.children
end

(* Not yet an actual AST, actually an ASL :) *)
type t = CommandGroup.t list

let to_list cg = cg

let find_matching_datafile datafile_map fpath =
  match List.assoc_opt fpath datafile_map with
  | Some p -> Some p
  | None ->
      (* No full match, but can we find a prefix dir *)
      List.fold_left
        (fun acc (_ipath, df) ->
          match acc with
          | Some x -> Some x
          | None -> (
              match Datafile.is_dir df with
              | false -> None
              | true -> (
                  match Fpath.rem_prefix (Datafile.path df) fpath with
                  | None -> None
                  | Some subpath ->
                      Some
                        (Datafile.v ~subpath:(Fpath.to_string subpath)
                           (Datafile.id df) (Datafile.path df)))))
        None datafile_map

let order_command_list metadata command_groups =
  let input_map =
    List.mapi
      (fun i f ->
        let df = Datafile.v i f in
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

              let style : Leaf.style =
                match List.exists Datafile.is_wildcard inputs with
                | true -> Map
                | false -> Command
              in

              let outputs =
                List.filter_map
                  (fun fpath ->
                    match find_matching_datafile datafile_map fpath with
                    | None ->
                        let id = !counter in
                        counter := !counter + 1;
                        Some (Datafile.v id fpath)
                    | Some _ -> None)
                  file_args
              in

              let x = Leaf.v !counter hd style inputs outputs in
              counter := !counter + 1;
              let updated_map, rest =
                loop tl
                  (List.concat
                     [
                       datafile_map;
                       List.map (fun o -> (Datafile.path o, o)) outputs;
                     ])
              in
              (updated_map, x :: rest)
        in
        let updated_map, commands = loop commands input_map in
        (updated_map, CommandGroup.v name commands))
      input_map command_groups
  in
  ordered
