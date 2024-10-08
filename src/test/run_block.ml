open Shark_ast

let test_initial_block () =
  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash es);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir es);
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es)

let null_runner _es _l _m _s _b = Ok "null runner"

let test_simple_change_dir () =
  let raw_command = "cd /data/arg1" in
  let command = Command.of_string raw_command in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ] in
  let command_leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs [] in

  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir es);

  let updated =
    Shark.Run_block.process_single_command_execution ~previous_state:es
      ~environment_override:[] ~command_leaf ~file_joins:[] ~file_subs_map:[]
      ~run_f:null_runner raw_command
  in
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash updated);
  Alcotest.(check string)
    "Check path" "/data/arg1"
    (Shark.Run_block.ExecutionState.workdir updated);
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env updated)

let test_simple_env_udpate () =
  let raw_command = "export SOMEKEY=SOMEVALUE" in
  let command = Command.of_string raw_command in
  let command_leaf = Leaf.v 42 (Option.get command) Leaf.Command [] [] in

  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es);

  let updated =
    Shark.Run_block.process_single_command_execution ~previous_state:es
      ~environment_override:[] ~command_leaf ~file_joins:[] ~file_subs_map:[]
      ~run_f:null_runner raw_command
  in
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash updated);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir updated);
  Alcotest.(check (list (pair string string)))
    "Check env"
    [ ("SOMEKEY", "SOMEVALUE") ]
    (Shark.Run_block.ExecutionState.env updated)

let test_override_env_udpate () =
  let raw_command = "export SOMEKEY=SOMEVALUE" in
  let command = Command.of_string raw_command in
  let command_leaf = Leaf.v 42 (Option.get command) Leaf.Command [] [] in

  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es);

  let environment_override = [ ("SOMEKEY", "OTHERVALUE") ] in

  let updated =
    Shark.Run_block.process_single_command_execution ~previous_state:es
      ~environment_override ~command_leaf ~file_joins:[] ~file_subs_map:[]
      ~run_f:null_runner raw_command
  in
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash updated);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir updated);
  Alcotest.(check (list (pair string string)))
    "Check env"
    [ ("SOMEKEY", "OTHERVALUE") ]
    (Shark.Run_block.ExecutionState.env updated)

let test_simple_command_execute () =
  let raw_command = "mycommand.exe" in
  let command = Command.of_string raw_command in
  let command_leaf = Leaf.v 42 (Option.get command) Leaf.Command [] [] in

  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in

  let runner_called = ref false in
  let runner _es _l _m _s _b =
    runner_called := true;
    Ok "otherid"
  in

  let updated =
    Shark.Run_block.process_single_command_execution ~previous_state:es
      ~environment_override:[] ~command_leaf ~file_joins:[] ~file_subs_map:[]
      ~run_f:runner raw_command
  in
  Alcotest.(check bool)
    "Check success" true
    (Shark.Run_block.ExecutionState.success updated);
  Alcotest.(check string)
    "Check id" "otherid"
    (Shark.Run_block.ExecutionState.build_hash updated);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir updated);
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env updated);
  Alcotest.(check bool) "check runner was called" true !runner_called

let test_simple_failed_command_execute () =
  let raw_command = "mycommand.exe" in
  let command = Command.of_string raw_command in
  let command_leaf = Leaf.v 42 (Option.get command) Leaf.Command [] [] in

  let es =
    Shark.Run_block.ExecutionState.init ~build_hash:"id" ~workdir:"/some/path"
      ~environment:[]
  in

  let runner_called = ref false in
  let runner _es _l _m _s _b =
    runner_called := true;
    Error (None, "error")
  in

  let updated =
    Shark.Run_block.process_single_command_execution ~previous_state:es
      ~environment_override:[] ~command_leaf ~file_joins:[] ~file_subs_map:[]
      ~run_f:runner raw_command
  in
  Alcotest.(check bool)
    "Check success" false
    (Shark.Run_block.ExecutionState.success updated);
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash updated);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir updated);
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env updated);
  Alcotest.(check bool) "check runner was called" true !runner_called

let tests =
  [
    ("Test create initial state", `Quick, test_initial_block);
    ("Test simple change dir", `Quick, test_simple_change_dir);
    ("Test simple env update", `Quick, test_simple_env_udpate);
    ( "Test env update with command line override",
      `Quick,
      test_override_env_udpate );
    ("Test a simple command execution", `Quick, test_simple_command_execute);
    ( "Test a failed command execution",
      `Quick,
      test_simple_failed_command_execute );
  ]
