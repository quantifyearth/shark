open Astring
module DataFile = Ordered_command.DataFile
module DataFileSet = Set.Make (DataFile)

(* In theory this could be a recursive structure that attempts to maintain the
   heirarchy of the document, markdown doesn't enforce that the section levels
   make any sort of sense, so for now I'm just going to assume a single level.

   I did initially try to implement that but got a lot of complexity for little
  initial benefit. *)
type section_group = {
  name : string;
  children : Block.t list;
}

let render_command_to_dot ppf command =
  (* let node_style = process_style node.style in *)
  (* TODO - some commands like littlejohn get different box styles*)
  let process_index = Ordered_command.Leaf.id command in
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n"
        (DataFile.id datafile) process_index)
    (Ordered_command.Leaf.inputs command);
  Format.fprintf ppf "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index "box"
    (Uri.pct_encode (Command.name (Ordered_command.Leaf.command command)));
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index
        (DataFile.id datafile))
    (Ordered_command.Leaf.outputs command);
  Format.fprintf ppf "\n"

let datafile_to_dot ppf datafile =
  Format.fprintf ppf "\tn%d[shape=\"cylinder\",label=\"%s\"];\n"
    (DataFile.id datafile) (DataFile.path datafile)

let render_ast_to_dot ppf (ast : Ordered_command.t) : unit =
  Format.fprintf ppf "digraph{\n";
  List.concat_map
    (fun group ->
      let commands = Ordered_command.CommandGroup.children group in
      List.concat_map (fun command ->
        let inputs = Ordered_command.Leaf.inputs command
        and outputs = Ordered_command.Leaf.outputs command in
        List.concat [ inputs; outputs ]) commands
    ) ast
  |> DataFileSet.of_list
  |> DataFileSet.iter (datafile_to_dot ppf);

  List.iteri (fun i group ->
    let name = Ordered_command.CommandGroup.name group
    and commands = Ordered_command.CommandGroup.children group in
    Format.fprintf ppf "subgraph \"cluster_%d\" {\n" i;
    Format.fprintf ppf "\tlabel = \"%s\"\n" name;
    List.iter (render_command_to_dot ppf) commands;
    Format.fprintf ppf "}\n"
  ) ast;
  Format.fprintf ppf "}\n"

let parse_frontmatter frontmatter =
  match Frontmatter.of_string frontmatter with
  | Ok frontmatter -> frontmatter
  | Error (`Msg m) -> failwith ("Failed to parse frontmatter: " ^ m)

let default ~info ~body = Some (Block.v ~alias:info ~body `Run)

let parse_markdown markdown =
  let doc = Cmarkit.Doc.of_string markdown in

  let current_section_title = ref "Top level"
  and cuirrent_block_list = ref []
  and sections = ref [] in

  let block _ = function
    | Cmarkit.Block.Heading (node, _meta) ->
      (* let level = Cmarkit.Block.Heading.level node in *)
      let title = Cmarkit.Block.Heading.inline node
      |> Cmarkit.Inline.to_plain_text ~break_on_soft:false
      |> List.map (String.concat ~sep:"")
      |> String.concat ~sep:" / " in

      (* bodge *)
      if (List.length !cuirrent_block_list) > 0 then (
        let order_corrected_group = List.rev !cuirrent_block_list in
        sections := { name = !current_section_title ; children = order_corrected_group} :: !sections
      );
      cuirrent_block_list := [];
      current_section_title := title;
      `Default
    | Cmarkit.Block.Code_block (node, _meta) ->
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
        (match Block.of_info_string ~default ~body info_str with
        | Some b -> cuirrent_block_list := b :: !cuirrent_block_list
        | None -> ());
        `Default
    | _ -> `Default
  in

  let mapper = Cmarkit.Mapper.make ~block () in
  ignore (Cmarkit.Mapper.map_doc mapper doc);

  (* Flush last section *)
    if (List.length !cuirrent_block_list) > 0 then (
      let order_corrected_group = List.rev !cuirrent_block_list in
      sections := { name = !current_section_title ; children = order_corrected_group} :: !sections
    );

  List.rev !sections

let render ~template_markdown =
  let metadata, sections =
    match String.cuts ~sep:"---" template_markdown with
    | [ frontmatter; markdown ] | [ ""; frontmatter; markdown ] ->
        (parse_frontmatter frontmatter, parse_markdown markdown)
    | [ markdown ] -> (Frontmatter.empty, parse_markdown markdown)
    | _ -> failwith "Malformed frontmatter/markdown file"
  in
  (* Initially ignore block scoping *)
  let s : (string * (Command.t list)) list =
  List.map (fun (sgroup : section_group) : (string * (Command.t list)) ->
    let a : string list list = List.map Block.command_list sgroup.children in
    let b : string list = List.concat a in
    let c : Command.t list = List.filter_map Command.of_string b in
    (
      sgroup.name, c
    )
  ) sections
  in
  let z : Ordered_command.t = Ordered_command.order_command_list metadata s in
  render_ast_to_dot Format.str_formatter z;
  Format.flush_str_formatter ()
