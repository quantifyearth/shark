open Astring
open Sexplib.Conv
module DatafileSet = Set.Make (Datafile)

module Astblock = struct
  type t = {
    hash : string list ref;
    context : string;
    block : Block.t;
    commands : Leaf.t list;
  }
  [@@deriving sexp]

  let v context block commands = { hash = ref []; context; block; commands }
  let block h = h.block
  let commands h = h.commands
  let context h = h.context
  let digest h = Block.digest h.block
  let hash h = match !(h.hash) with [] -> None | hd :: _ -> Some hd
  let hashes h = !(h.hash)
  let update_hash h hash = h.hash := hash :: !(h.hash)

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
      DatafileSet.to_list all_outputs )

  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
end

module Section = struct
  type t = { name : string; blocks : Astblock.t list }

  let v name blocks = { name; blocks }
  let name s = s.name
  let blocks s = s.blocks
end

type block_id = int [@@deriving sexp]

type t = {
  nodes : (block_id * Astblock.t) list;
  edges : (block_id * block_id) list;
  metadata : Frontmatter.t;
}
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)

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
        match Block.of_code_block node with
        | None -> Cmarkit.Folder.default
        | Some b -> (
            match Block.kind b with
            | _ -> (
                match acc with
                | [] ->
                    Cmarkit.Folder.ret
                      [ { name = "Top level"; children = [ b ] } ]
                | hd :: tl ->
                    Cmarkit.Folder.ret
                      ({
                         name = hd.name;
                         children = List.rev (b :: List.rev hd.children);
                       }
                      :: tl))))
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
      Block.command_list block |> List.filter_map Command.of_string
      (* |> List.filter_map (fun c ->
             match Command.file_args c with [] -> None | _ -> Some c); *);
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
              (updated_map, Astblock.v name superblock.block leaves))
            input_map superblocks
        in
        (updated_map, Section.v name processed_section))
      input_map section_list
  in
  processed

(* ----- public interface ----- *)

let to_list ast = List.map snd ast.nodes

let synthesize_import_block input_map input_override_map =
  let imports =
    List.map
      (fun (k, p) ->
        let dest = List.assoc k input_map in
        (p, dest))
      input_override_map
  in
  let codeblock =
    List.fold_left
      (fun acc (src, dst) ->
        acc
        ^ Printf.sprintf "%s %s\n" (Fpath.to_string src) (Fpath.to_string dst))
      "" imports
  in
  let block = Block.import codeblock in
  ("imports", [ block_to_superblock block ])

let synthesize_unmapped_import_block input_map =
  let codeblock =
    List.fold_left
      (fun acc (src, dst) ->
        acc ^ Printf.sprintf "%s %s\n" src (Fpath.to_string dst))
      "" input_map
  in
  let block = Block.import codeblock in
  ("imports", [ block_to_superblock block ])

let of_sharkdown ?concrete_paths template_markdown =
  let metadata, sections, markdown =
    match String.cuts ~sep:"---" template_markdown with
    | [ frontmatter; markdown ] | [ ""; frontmatter; markdown ] ->
        (parse_frontmatter frontmatter, parse_markdown markdown, markdown)
    | [ markdown ] -> (Frontmatter.empty, parse_markdown markdown, markdown)
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

  let input_map = Frontmatter.input_map metadata in
  let synthesized_sections =
    match input_map with
    | [] -> []
    | _ -> (
        match concrete_paths with
        | Some concrete_paths ->
            [ synthesize_import_block input_map concrete_paths ]
        | None -> [ synthesize_unmapped_import_block input_map ])
  in

  let expanded_markdown =
    List.fold_left
      (fun acc (name, bs) ->
        let title = Printf.sprintf "# %s\n\n" name in
        let body =
          List.fold_left
            (fun acc b ->
              Printf.sprintf "```%s\n%s\n```\n\n"
                (Block.to_info_string b.block)
                (Block.body b.block)
              ^ acc)
            "\n" bs
        in

        (title ^ body) ^ acc)
      markdown synthesized_sections
  in

  let expanded_sections = synthesized_sections @ detailed_sections in

  (* we can only infer the dependancy graph globally, so we need to do this at the top level before
     then working out the DAG. *)
  let pass1 = pass_one_on_list [] expanded_sections in

  (* Now I have the global graph implicitly, turn the list into a graph of blocks *)
  let all_astblocks = List.concat_map Section.blocks pass1 in
  let id_all_astblocks = List.mapi (fun i h -> (i, h)) all_astblocks in

  (* All files will have one writer and zero or more readers *)
  let writers =
    List.concat
      (List.map
         (fun (hbid, h) ->
           let _, outputs = Astblock.io h in
           List.map (fun o -> (Datafile.id o, (o, hbid))) outputs)
         id_all_astblocks)
  in

  let edges =
    List.concat
      (List.map
         (fun (hbid, h) ->
           let inputs, _ = Astblock.io h in
           List.filter_map
             (fun i ->
               match List.assoc_opt (Datafile.id i) writers with
               | None -> None
               | Some (_, writerid) -> Some (writerid, hbid))
             inputs)
         id_all_astblocks)
  in

  ({ nodes = id_all_astblocks; edges; metadata }, expanded_markdown)

let find_id_of_block ast ib =
  let d = Block.digest ib in
  let rec loop l =
    match l with
    | [] -> None
    | hd :: tl ->
        let id, hb = hd in
        let b = Astblock.block hb in
        if Block.digest b = d then Some id else loop tl
  in
  loop ast.nodes

let block_by_id ast id = List.assoc_opt id ast.nodes

let find_ast_block_from_shark_block ast block =
  let id = find_id_of_block ast block in
  Option.bind id (block_by_id ast)

let find_dependencies ast id =
  List.filter_map
    (fun (edge : block_id * block_id) : block_id option ->
      let from, too = edge in
      if too = id then Some from else None)
    ast.edges
  |> List.sort_uniq (fun a b -> a - b)
     (* remove duplicates if we take more than one output from a block *)
  |> List.map (fun id -> List.assoc id ast.nodes)

let default_container_path ast = Frontmatter.default_container_path ast.metadata
