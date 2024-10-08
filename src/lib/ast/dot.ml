module DatafileSet = Set.Make (Datafile)

let render_command_to_dot ppf command =
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

let render_publish_to_dot ppf command =
  let process_index = Leaf.id command in
  (* In publish blocks the targets end up as commands where the name is the target *)
  Format.fprintf ppf "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" process_index
    (Command.name (Leaf.command command));
  List.iter
    (fun datafile ->
      let label =
        match Datafile.subpath datafile with
        | Some x -> Fmt.str ",label=\"%s\"" x
        | None -> ""
      in
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"%s];\n"
        (Datafile.id datafile) process_index label)
    (Leaf.inputs command)

let render_import_to_dot ppf command =
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
  Format.fprintf ppf "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" process_index
    (Uri.pct_encode (Command.name (Leaf.command command)));
  List.iter
    (fun datafile ->
      Format.fprintf ppf "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index
        (Datafile.id datafile))
    (Leaf.outputs command);
  Format.fprintf ppf "\n"

let datafile_to_dot ppf datafile =
  let shape =
    match Datafile.is_dir datafile with true -> "tab" | false -> "note"
  in
  Format.fprintf ppf "\tn%d[shape=\"%s\",label=\"%s\"];\n"
    (Datafile.id datafile) shape
    (Fpath.to_string (Datafile.path datafile))

let render_ast_to_dot ppf astblocks : unit =
  Format.fprintf ppf "digraph{\n";
  List.concat_map
    (fun hb ->
      let commands = Block.commands hb in
      List.concat_map
        (fun command ->
          let inputs = Leaf.inputs command and outputs = Leaf.outputs command in
          List.concat [ inputs; outputs ])
        commands)
    astblocks
  |> DatafileSet.of_list
  |> DatafileSet.iter (datafile_to_dot ppf);

  List.iteri
    (fun i hb ->
      let kind = Block.Raw.kind (Block.raw hb) in
      let name, style =
        match kind with
        | `Publish -> ("Publish", "bold")
        | _ -> (Block.context hb, "solid")
      and commands = Block.commands hb in
      Format.fprintf ppf "subgraph \"cluster_%d\" {\n" i;
      Format.fprintf ppf "\tstyle = %s\n" style;
      Format.fprintf ppf "\tlabel = \"%s\"\n" name;

      (* if commands have no obvious I/O remove them from the dot graph for now *)
      let filtered_commands =
        List.filter
          (fun l ->
            let ic = List.length (Leaf.inputs l)
            and oc = List.length (Leaf.outputs l) in
            ic + oc > 0)
          commands
      in

      let renderer =
        match kind with
        | `Import -> render_import_to_dot
        | `Run -> render_command_to_dot
        | `Publish -> render_publish_to_dot
        | _ -> fun _a _b -> ()
      in
      List.iter (renderer ppf) filtered_commands;

      Format.fprintf ppf "}\n")
    astblocks;
  Format.fprintf ppf "}\n"

let render ppf ast = Ast.to_list ast |> render_ast_to_dot ppf
