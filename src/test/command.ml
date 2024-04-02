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
    ("Basic python command parsing for file", `Quick, test_python_command_direct);
    ("Basic R command parsing", `Quick, test_rscript_command_basic);
    ( "Basic R command parsing with options",
      `Quick,
      test_rscript_command_options );
    ("Basic command parsing", `Quick, test_generic_command_basic);
    ( "Basic command parsing with prefix",
      `Quick,
      test_generic_command_basic_with_prefix );
  ]
