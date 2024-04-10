module DatafileSet = Set.Make (Datafile)

let render_command_to_dot ppf command =
  (* let node_style = process_style node.style in *)
  (* TODO - some commands like littlejohn get different box styles*)
  let process_index = Leaf.id command in
  List.iter
    (fun datafile ->
      let label =
        match Datafile.subpath datafile with
        | Some x -> Fmt.str ",label=\"%s\"" x
        | None -> ""
      in
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"%s];\n"
        (Datafile.id datafile) process_index label)
    (Leaf.inputs command);
  let shape =
    match Leaf.command_style command with Command -> "box" | Map -> "box3d"
  in
  Format.fprintf ppf "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index shape
    (Uri.pct_encode (Command.name (Leaf.command command)));
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index
        (Datafile.id datafile))
    (Leaf.outputs command);
  Format.fprintf ppf "\n"

let datafile_to_dot ppf datafile =
  Format.fprintf ppf "\tn%d[shape=\"cylinder\",label=\"%s\"];\n"
    (Datafile.id datafile)
    (Fpath.to_string (Datafile.path datafile))

let render_ast_to_dot ppf ast : unit =
  Format.fprintf ppf "digraph{\n";
  List.concat_map
    (fun group ->
      let commands = Commandgroup.children group in
      List.concat_map
        (fun command ->
          let inputs = Leaf.inputs command and outputs = Leaf.outputs command in
          List.concat [ inputs; outputs ])
        commands)
    ast
  |> DatafileSet.of_list
  |> DatafileSet.iter (datafile_to_dot ppf);

  List.iteri
    (fun i group ->
      let name = Commandgroup.name group
      and commands = Commandgroup.children group in
      Format.fprintf ppf "subgraph \"cluster_%d\" {\n" i;
      Format.fprintf ppf "\tlabel = \"%s\"\n" name;
      List.iter (render_command_to_dot ppf) commands;
      Format.fprintf ppf "}\n")
    ast;
  Format.fprintf ppf "}\n"

let render ~template_markdown =
  Ast.of_sharkdown ~template_markdown
  |> Ast.to_list
  |> render_ast_to_dot Format.str_formatter;
  Format.flush_str_formatter ()
