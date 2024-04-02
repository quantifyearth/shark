module Basic = struct
  let block = Alcotest.of_pp Shark.Block.pp

  let test_shark_block () =
    let build_string_no_hash = "shark-build:gdal-env" in
    let expect = Shark.Block.v ~alias:"gdal-env" ~body:"" `Build in
    let test = Shark.Block.of_info_string ~body:"" build_string_no_hash in
    Alcotest.(check (option block)) "same block" (Some expect) test;

    let build_string_hash = "shark-build:gdal-env:abcdefg" in
    let expect =
      Shark.Block.v ~hash:"abcdefg" ~alias:"gdal-env" ~body:"" `Build
    in
    let test = Shark.Block.of_info_string ~body:"" build_string_hash in
    Alcotest.(check (option block)) "same block hash" (Some expect) test;

    let ocaml = "ocaml" in
    let test = Shark.Block.of_info_string ~body:"" ocaml in
    Alcotest.(check (option block)) "same default block" None test

  let tests = [ ("shark blocks", `Quick, test_shark_block) ]
end

module CommandParsing = struct
  let command = Alcotest.of_pp Shark.Command.pp

  let test_python_command_module () =
    let testcase = "python3 -m some.module.code arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"some.module.code"
        ~args:[ "-m"; "some.module.code"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let test_python_command_direct () =
    let testcase = "python3 some/module/code.py arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"code.py"
        ~args:[ "some/module/code.py"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let test_rscript_command_basic () =
    let testcase = "Rscript some/module/code.r arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"code.r"
        ~args:[ "some/module/code.r"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let test_rscript_command_options () =
    let testcase = "Rscript --no-environ --save some/module/code.r arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"code.r"
        ~args:[ "--no-environ"; "--save"; "some/module/code.r"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let test_generic_command_basic () =
    let testcase = "docker arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"docker"
        ~args:[ "docker"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let test_generic_command_basic_with_prefix () =
    let testcase = "$ docker arg1 arg2" in
    let expected =
      Shark.Command.v ~name:"docker"
        ~args:[ "$"; "docker"; "arg1"; "arg2" ]
        ~file_args:[]
    in
    let test = Shark.Command.of_string testcase in
    Alcotest.(check (option command)) "Command" test (Some expected)

  let tests =
    [
      ( "Basic python command parsing for module",
        `Quick,
        test_python_command_module );
      ( "Basic python command parsing for file",
        `Quick,
        test_python_command_direct );
      ("Basic R command parsing", `Quick, test_rscript_command_basic);
      ( "Basic R command parsing with options",
        `Quick,
        test_rscript_command_options );
      ("Basic command parsing", `Quick, test_generic_command_basic);
      ( "Basic command parsing with prefix",
        `Quick,
        test_generic_command_basic_with_prefix );
    ]
end

module DataFile = struct
  (* let datafile = Alcotest.of_pp Shark.Ast.DataFile.pp *)

  let test_basic_file_path () =
    let testcase = "/data/test/example.tif" in
    let test = Shark.Ast.DataFile.v 42 testcase in
    Alcotest.(check int) "Same id" 42 (Shark.Ast.DataFile.id test);
    Alcotest.(check string) "Same path" testcase (Shark.Ast.DataFile.path test);
    Alcotest.(check (option string))
      "No subpath" None
      (Shark.Ast.DataFile.subpath test);
    Alcotest.(check bool)
      "Isn't wildcard" false
      (Shark.Ast.DataFile.is_wildcard test);
    Alcotest.(check bool) "Isn't dir" false (Shark.Ast.DataFile.is_dir test)

  let test_basic_dir_noncanonical () =
    let testcase = "/data/test" in
    let test = Shark.Ast.DataFile.v 42 testcase in
    Alcotest.(check int) "Same id" 42 (Shark.Ast.DataFile.id test);
    Alcotest.(check string)
      "Extended path" "/data/test/"
      (Shark.Ast.DataFile.path test);
    Alcotest.(check (option string))
      "No subpath" None
      (Shark.Ast.DataFile.subpath test);
    Alcotest.(check bool)
      "Isn't wildcard" false
      (Shark.Ast.DataFile.is_wildcard test);
    Alcotest.(check bool) "Is dir" true (Shark.Ast.DataFile.is_dir test)

  let test_basic_dir_canonical () =
    let testcase = "/data/test/" in
    let test = Shark.Ast.DataFile.v 42 testcase in
    Alcotest.(check int) "Same id" 42 (Shark.Ast.DataFile.id test);
    Alcotest.(check string) "Same path" testcase (Shark.Ast.DataFile.path test);
    Alcotest.(check (option string))
      "No subpath" None
      (Shark.Ast.DataFile.subpath test);
    Alcotest.(check bool)
      "Isn't wildcard" false
      (Shark.Ast.DataFile.is_wildcard test);
    Alcotest.(check bool) "Is dir" true (Shark.Ast.DataFile.is_dir test)

  let test_basic_dir_canonical_with_wildcard () =
    let testcase = "/data/test/" in
    let test = Shark.Ast.DataFile.v ~subpath:"*" 42 testcase in
    Alcotest.(check int) "Same id" 42 (Shark.Ast.DataFile.id test);
    Alcotest.(check string)
      "Extended path" "/data/test/"
      (Shark.Ast.DataFile.path test);
    Alcotest.(check (option string))
      "No subpath" None
      (Shark.Ast.DataFile.subpath test);
    Alcotest.(check bool)
      "Is wildcard" true
      (Shark.Ast.DataFile.is_wildcard test);
    Alcotest.(check bool) "Is dir" true (Shark.Ast.DataFile.is_dir test)

  let tests =
    [
      ("Basic file", `Quick, test_basic_file_path);
      ("Non-canonical dir", `Quick, test_basic_dir_noncanonical);
      ("Canonical dir", `Quick, test_basic_dir_canonical);
      ( "Canonical dir with wildcard",
        `Quick,
        test_basic_dir_canonical_with_wildcard );
    ]
end

let () =
  Alcotest.run "shark"
    [
      ("basic", Basic.tests);
      ("command parsing", CommandParsing.tests);
      ("datafile modeling", DataFile.tests);
    ]
