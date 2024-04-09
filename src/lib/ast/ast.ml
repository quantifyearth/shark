open Astring
module DatafileSet = Set.Make (Datafile)

module Hyperblock = struct
  type t = { context : string; block : Block.t; commands : Leaf.t list }

  let v context block commands = { context; block; commands }
  let block h = h.block
  let commands h = h.commands
  let context h = h.context
  let digest h = Block.digest h.block

  let io h =
    let all_inputs, all_outputs =
      List.fold_left
        (fun acc v ->
          let inputs, outputs = acc in
          ( DatafileSet.union inputs (DatafileSet.of_list (Leaf.inputs v)),
            DatafileSet.union outputs (DatafileSet.of_list (Leaf.outputs v)) ))
        (DatafileSet.empty, DatafileSet.empty)
        h.commands
    in
    ( DatafileSet.to_list (DatafileSet.diff all_inputs all_outputs),
      DatafileSet.to_list (DatafileSet.diff all_outputs all_inputs) )
end

module Section = struct
  type t = { name : string; blocks : Hyperblock.t list }

  let v name blocks = { name; blocks }
  let name s = s.name
  let blocks s = s.blocks
end

type block_id = int

type dag = {
  nodes : (block_id * Hyperblock.t) list;
  edges : (block_id * block_id) list;
}

type t = dag

(* ----- front matter parser ----- *)

let parse_frontmatter frontmatter =
  match Frontmatter.of_string frontmatter with
  | Ok frontmatter -> frontmatter
  | Error (`Msg m) -> failwith ("Failed to parse frontmatter: " ^ m)

(* ----- markdown body parser ----- *)

(* In theory this could be a recursive structure that attempts to maintain the
   heirarchy of the document, markdown doesn't enforce that the section levels
   make any sort of sense, so for now I'm just going to assume a single level.

   I did initially try to implement that but got a lot of complexity for little
   initial benefit. *)
type section_group = { name : string; children : Block.t list }

let default ~info ~body = Some (Block.v ~alias:info ~body `Run)

let parse_markdown markdown =
  let doc = Cmarkit.Doc.of_string markdown in

  let block _ acc = function
    | Cmarkit.Block.Heading (node, _meta) ->
        let title =
          Cmarkit.Block.Heading.inline node
          |> Cmarkit.Inline.to_plain_text ~break_on_soft:false
          |> List.map (String.concat ~sep:"")
          |> String.concat ~sep:" / "
        in
        Cmarkit.Folder.ret ({ name = title; children = [] } :: acc)
    | Cmarkit.Block.Code_block (node, _meta) -> (
        let info_str =
          match Cmarkit.Block.Code_block.info_string node with
          | None -> "shark-run:"
          | Some (info_str, _) -> info_str
        in
        let body = Cmarkit.Block.Code_block.code node in
        let body =
          List.map Cmarkit.Block_line.to_string body
          |> List.map String.trim |> String.concat ~sep:"\n"
        in
        match Block.of_info_string ~default ~body info_str with
        | None -> Cmarkit.Folder.default
        | Some b -> (
            match acc with
            | [] ->
                Cmarkit.Folder.ret [ { name = "Top level"; children = [ b ] } ]
            | hd :: tl ->
                Cmarkit.Folder.ret
                  ({
                     name = hd.name;
                     children = List.rev (b :: List.rev hd.children);
                   }
                  :: tl)))
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block () in
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
  |> List.filter_map (fun x -> match x.children with [] -> None | _ -> Some x)

(* ----- internal tree building code ----- *)

type superblock = { block : Block.t; commands : Command.t list }

let block_to_superblock (block : Block.t) : superblock =
  {
    block;
    commands =
      Block.command_list block
      |> List.filter_map Command.of_string
      |> List.filter_map (fun c ->
             match Command.file_args c with [] -> None | _ -> Some c);
  }

let build_initial_input_map inputs =
  let input_map =
    List.mapi
      (fun i f ->
        let df = Datafile.v i f in
        (f, df))
      inputs
  in
  let counter = ref (List.length input_map) in
  (input_map, counter)

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

let rec pass_one_process_commands_loop counter commands datafile_map =
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
        pass_one_process_commands_loop counter tl
          (List.concat
             [ datafile_map; List.map (fun o -> (Datafile.path o, o)) outputs ])
      in
      (updated_map, x :: rest)

let pass_one_on_list inputs section_list =
  let input_map, counter = build_initial_input_map inputs in
  let _, processed =
    List.fold_left_map
      (fun input_map section ->
        let name, superblocks = section in
        let updated_map, processed_section =
          List.fold_left_map
            (fun input_map superblock ->
              let updated_map, leaves =
                pass_one_process_commands_loop counter superblock.commands
                  input_map
              in
              (updated_map, Hyperblock.v name superblock.block leaves))
            input_map superblocks
        in
        (updated_map, Section.v name processed_section))
      input_map section_list
  in
  processed

(* ----- public interface ----- *)

let to_list ast =
  (* in days of yore this was by section, but for now we do it by executable block *)
  List.map
    (fun x ->
      let _hbid, h = x in
      Commandgroup.v (Hyperblock.context h) (Hyperblock.commands h))
    ast.nodes

let of_sharkdown ~template_markdown =
  let metadata, sections =
    match String.cuts ~sep:"---" template_markdown with
    | [ frontmatter; markdown ] | [ ""; frontmatter; markdown ] ->
        (parse_frontmatter frontmatter, parse_markdown markdown)
    | [ markdown ] -> (Frontmatter.empty, parse_markdown markdown)
    | _ -> failwith "Malformed frontmatter/markdown file"
  in

  (* Now I have a list of sections with a name and a list of blocks. The list nature has an
     implicit dependnacy order, but that's wrong, so we need to turn this into a DAG of blocks.

     1: Markdown -> list of sections, where a section is a name and a list of blocks.
     2: -> List of sections, where a section is a list a name, and a list of block/commands per block
     3: -> build the data dependnacy graph
  *)
  let detailed_sections =
    List.map
      (fun sgroup ->
        (sgroup.name, List.map block_to_superblock sgroup.children))
      sections
  in

  (* we can only infer the dependancy graph globally, so we need to do this at the top level before
     then working out the DAG. *)
  let pass1 =
    pass_one_on_list (Frontmatter.inputs metadata) detailed_sections
  in

  (* Now I have the global graph implicitly, turn the list into a graph of blocks *)
  let all_hyperblocks = List.concat_map Section.blocks pass1 in
  let id_all_hyperblocks = List.mapi (fun i h -> (i, h)) all_hyperblocks in

  (* All files will have one writer and zero or more readers *)
  let writers =
    List.concat
      (List.map
         (fun x ->
           let hbid, h = x in
           let _, outputs = Hyperblock.io h in
           List.map (fun o -> (o, hbid)) outputs)
         id_all_hyperblocks)
  in

  let edges =
    List.concat
      (List.map
         (fun x ->
           let hbid, h = x in
           let inputs, _ = Hyperblock.io h in
           List.filter_map
             (fun i ->
               match List.assoc_opt i writers with
               | None -> None
               | Some wid -> Some (wid, hbid))
             inputs)
         id_all_hyperblocks)
  in

  { nodes = id_all_hyperblocks; edges }

let find_id_of_block ast b =
  let d = Block.digest b in
  let rec loop l =
    match l with
    | [] -> None
    | hd :: tl ->
        let id, hb = hd in
        let b = Hyperblock.block hb in
        if Block.digest b = d then Some id else loop tl
  in
  loop ast.nodes

let find_dependancies ast id =
  List.filter_map
    (fun (edge : block_id * block_id) : block_id option ->
      let from, too = edge in
      if too = id then Some from else None)
    ast.edges
  |> List.map (fun id -> List.assoc id ast.nodes)
