open Astring
module DataFile = Ordered_command.DataFile
module DataFileSet = Set.Make (DataFile)

let process_to_dot ppf command =
  (* let node_style = process_style node.style in *)
  (* TODO - some commands like littlejohn get different box styles*)
  let process_index = Ordered_command.id command in
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n"
        (DataFile.id datafile) process_index)
    (Ordered_command.inputs command);
  Format.fprintf ppf "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index "box"
    (Uri.pct_encode (Command.name (Ordered_command.command command)));
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index
        (DataFile.id datafile))
    (Ordered_command.outputs command);
  Format.fprintf ppf "\n"

let datafile_to_dot ppf datafile =
  Format.fprintf ppf "\tn%d[shape=\"cylinder\",label=\"%s\"];\n"
    (DataFile.id datafile) (DataFile.path datafile)

let project_to_dot ppf (project : Ordered_command.t list) : unit =
  Format.fprintf ppf "digraph{\n";
  List.concat_map
    (fun command ->
      let inputs = Ordered_command.inputs command
      and outputs = Ordered_command.outputs command in
      List.concat [ inputs; outputs ])
    project
  |> DataFileSet.of_list
  |> DataFileSet.iter (datafile_to_dot ppf);

  List.iter (process_to_dot ppf) project;
  Format.fprintf ppf "}\n"

let parse_frontmatter frontmatter =
  match Frontmatter.of_string frontmatter with
  | Some frontmatter -> frontmatter
  | None -> failwith "Failed to parse frontmatter\n"

let parse_markdown (markdown : string) : Block.t list =
  let doc = Cmarkit.Doc.of_string markdown in

  let blocks = ref [] in

  let block _ = function
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
        (match Block.of_info_string ~body info_str with
        | Some b -> blocks := b :: !blocks
        | None -> ());
        `Default
    | _ -> `Default
  in

  let mapper = Cmarkit.Mapper.make ~block () in
  ignore (Cmarkit.Mapper.map_doc mapper doc);
  List.rev !blocks

let render ~template_markdown =
  let metadata, ast =
    match String.cuts ~sep:"---" template_markdown with
    | [ frontmatter; markdown ] | [ ""; frontmatter; markdown ] ->
        (parse_frontmatter frontmatter, parse_markdown markdown)
    | [ markdown ] -> (Frontmatter.empty, parse_markdown markdown)
    | _ -> failwith "Malformed frontmatter/markdown file"
  in
  (* Initially ignore block scoping *)
  List.map Block.command_list ast
  |> List.concat
  |> List.filter_map Command.of_string
  |> Ordered_command.order_command_list metadata
  |> project_to_dot Format.str_formatter;
  Format.flush_str_formatter ()
