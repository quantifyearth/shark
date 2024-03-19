open Astring
open Block
open Command
open Frontmatter
open Ordered_command

module DataFileSet = Set.Make(DataFile)

let process_to_dot command =
  (* let node_style = process_style node.style in *) (* TODO - some commands like littlejohn get different box styles*)
  let process_index = OrderedCommand.id command in
  List.iter ( fun datafile ->
      Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" (DataFile.id datafile) process_index
  ) (OrderedCommand.inputs command);
  Printf.fprintf Stdlib.stdout "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index "box" (Uri.pct_encode (Command.name (OrderedCommand.command command)));
  List.iter ( fun datafile ->
    Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index (DataFile.id datafile)
) (OrderedCommand.outputs command);
  Printf.fprintf Stdlib.stdout "\n"


let datafile_to_dot datafile =
  Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" (DataFile.id datafile) (DataFile.path datafile)


let project_to_dot (project : OrderedCommand.t list) : unit =
  Printf.fprintf Stdlib.stdout "digraph{\n";

  List.concat_map (fun command ->
    let inputs = OrderedCommand.inputs command
    and outputs = OrderedCommand.outputs command in
    List.concat [ inputs ; outputs]
  ) project |> DataFileSet.of_list |>
  DataFileSet.iter datafile_to_dot;

  List.iter process_to_dot project ;

  Printf.fprintf Stdlib.stdout "}\n"

let parse_frontmatter frontmatter =
  match (Frontmatter.of_string frontmatter) with
  | Some frontmatter -> frontmatter
  | None -> failwith "Failed to parse frontmatter\n"

let parse_markdown (markdown : string) : Block.t list =
  let doc = Cmarkit.Doc.of_string markdown in

  let blocks = ref [] in

  let block _ = function
  | Cmarkit.Block.Code_block (node, _meta) -> (
      let info_str = match Cmarkit.Block.Code_block.info_string node with
      | None -> "shark-run:"
      | Some (info_str, _) -> info_str
      in
      let body = Cmarkit.Block.Code_block.code node in
      let body =
        ((List.map Cmarkit.Block_line.to_string body) |> List.map String.trim |> String.concat ~sep:"\n")
      in (
        match Block.of_info_string body info_str with
        | Some b -> blocks := b :: !blocks
        | None -> ()
      );
      `Default
  )
  | _ -> `Default
  in

  let mapper = Cmarkit.Mapper.make ~block () in
  ignore(Cmarkit.Mapper.map_doc mapper doc);
  List.rev !blocks

let render ~file_path : unit =
  let template = Eio.Path.load file_path in
  let metadata, ast = match String.cuts ~sep:"---" template with
  | [frontmatter; markdown] | [ ""; frontmatter; markdown ] ->
    (parse_frontmatter frontmatter, parse_markdown markdown)
  | [markdown] -> (Frontmatter.empty, parse_markdown markdown)
  | _ -> failwith "Malformed frontmatter/markdown file"
  in
  (* Initially ignore block scoping *)
  List.map Block.command_list ast |>
  List.concat |>
  List.filter_map Command.of_string |>
  OrderedCommand.order_command_list metadata |> project_to_dot
