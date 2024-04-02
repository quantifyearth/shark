open Astring
open Sexplib.Conv

module DataFile = struct
  type t = { id : int; path : string; subpath : string option; wildcard : bool }
  [@@deriving sexp]

  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)

  let v ?subpath id path =
    let wildcard =
      match subpath with
      | None -> false
      | Some p ->
          let last_char =
            String.Sub.to_string (String.sub ~start:(String.length p - 1) p)
          in
          last_char = "*"
    in
    if wildcard then
      {
        id;
        path = String.Sub.to_string (String.sub ~stop:(String.length path) path);
        subpath = None;
        wildcard = true;
      }
    else { id; path; subpath; wildcard = false }

  let id d = d.id

  let path d =
    match Filename.extension d.path = "" with
    | false -> d.path
    | true ->
        let p = d.path in
        let last_char = String.sub ~start:(String.length p - 1) p in
        if String.Sub.to_string last_char = "/" then p else p ^ "/"

  let path_nc d = d.path
  let subpath d = d.subpath
  let is_wildcard d = d.wildcard
  let compare a b = Int.compare a.id b.id

  let is_dir d =
    (* a little hacky, we probably need to do something in the sharkdown here *)
    Filename.extension d.path = ""
end

module Leaf = struct
  type style = Command | Map [@@deriving sexp]

  type t = {
    id : int;
    command : Command.t;
    style : style;
    inputs : DataFile.t list;
    outputs : DataFile.t list;
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
                        (DataFile.v
                           ~subpath:
                             (String.Sub.to_string
                                (String.sub ~start:(String.length ipath) path))
                           (DataFile.id df) (DataFile.path df))
                  | false -> None)))
        None datafile_map

let order_command_list metadata command_groups =
  let input_map =
    List.mapi
      (fun i f ->
        let df = DataFile.v i f in
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
                match List.exists DataFile.is_wildcard inputs with
                | true -> Map
                | false -> Command
              in

              let outputs =
                List.filter_map
                  (fun path ->
                    match find_matching_datafile datafile_map path with
                    | None ->
                        let id = !counter in
                        counter := !counter + 1;
                        Some (DataFile.v id path)
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
                       List.map (fun o -> (DataFile.path_nc o, o)) outputs;
                     ])
              in
              (updated_map, x :: rest)
        in
        let updated_map, commands = loop commands input_map in
        (updated_map, CommandGroup.v name commands))
      input_map command_groups
  in
  ordered
