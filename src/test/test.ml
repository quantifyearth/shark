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

  let test_python_command_module () =
    let testcase = "python3 -m some.module.code arg1 arg2" in
    let expected_name = "some.module.code" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let test_python_command_direct () =
    let testcase = "python3 some/module/code.py arg1 arg2" in
    let expected_name = "code.py" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let test_rscript_command_basic () =
    let testcase = "Rscript some/module/code.r arg1 arg2" in
    let expected_name = "code.r" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let test_rscript_command_options () =
    let testcase = "Rscript --no-environ --save some/module/code.r arg1 arg2" in
    let expected_name = "code.r" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let test_generic_command_basic () =
    let testcase = "docker arg1 arg2" in
    let expected_name = "docker" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let test_generic_command_basic_with_prefix () =
    let testcase = "$ docker arg1 arg2" in
    let expected_name = "docker" in

    let test = Shark.Command.of_string testcase in
    match test with
    | None -> Alcotest.fail "Command parse failed"
    | Some t -> Alcotest.(check string) "Command name" (Shark.Command.name t) expected_name

  let tests = [
    ("Basic python command parsing for module", `Quick, test_python_command_module) ;
    ("Basic python command parsing for file", `Quick, test_python_command_direct) ;
    ("Basic R command parsing", `Quick, test_rscript_command_basic) ;
    ("Basic R command parsing with options", `Quick, test_rscript_command_options) ;
    ("Basic command parsing", `Quick, test_generic_command_basic) ;
    ("Basic command parsing with prefix", `Quick, test_generic_command_basic_with_prefix) ;
  ]

end

let () = Alcotest.run "shark" [ ("basic", Basic.tests) ; ("command parsing", CommandParsing.tests) ]
