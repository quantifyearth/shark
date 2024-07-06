let () =
  Alcotest.run "shark"
    [
      ("block", Block.tests);
      ("command parsing", Command.tests);
      ("datafile modeling", Datafile.tests);
      ("frontmatter parsing", Frontmatter.tests);
      ("AST parsing", Ast.tests);
      ("AST leaf processing", Leaf.tests);
      ("Command execution", Run_block.tests);
    ]
