let () =
  Alcotest.run "shark"
    [
      ("basic", Basic.tests);
      ("command parsing", Command.tests);
      ("datafile modeling", Datafile.tests);
      ("frontmatter parsing", Frontmatter.tests);
    ]
