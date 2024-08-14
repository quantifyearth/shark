let () =
  let tmf = In_channel.with_open_bin "./tmf.md" In_channel.input_all in
  let ast, _ = Shark.Md_to_ast.of_sharkdown tmf in
  Shark_ast.Ast.pp_dot Fmt.stdout ast
