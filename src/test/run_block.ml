let test_initial_block () =
  let es = Shark.Run_block.ExecutionState.init "id" "/some/path" [] in
  Alcotest.(check string)
    "Check id" "id"
    (Shark.Run_block.ExecutionState.build_hash es);
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir es);
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es)

let null_runner _es _l _s _b = Ok "null runner"

let test_simple_change_dir () =
  let raw_command = "cd /data/arg1" in
  let command = Shark.Command.of_string raw_command in
  let inputs = [ Shark.Datafile.v 0 (Fpath.v "/data/arg1") ] in
  let leaf =
    Shark.Leaf.v 42 (Option.get command) Shark.Leaf.Command inputs []
  in

  let es = Shark.Run_block.ExecutionState.init "id" "/some/path" [] in
  Alcotest.(check string)
    "Check path" "/some/path"
    (Shark.Run_block.ExecutionState.workdir es);

  let updated =
    Shark.Run_block.process_single_command_execution es [] leaf [] null_runner
      raw_command
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
  let command = Shark.Command.of_string raw_command in
  let leaf = Shark.Leaf.v 42 (Option.get command) Shark.Leaf.Command [] [] in

  let es = Shark.Run_block.ExecutionState.init "id" "/some/path" [] in
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es);

  let updated =
    Shark.Run_block.process_single_command_execution es [] leaf [] null_runner
      raw_command
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
  let command = Shark.Command.of_string raw_command in
  let leaf = Shark.Leaf.v 42 (Option.get command) Shark.Leaf.Command [] [] in

  let es = Shark.Run_block.ExecutionState.init "id" "/some/path" [] in
  Alcotest.(check (list (pair string string)))
    "Check env" []
    (Shark.Run_block.ExecutionState.env es);

  let env_override = [ ("SOMEKEY", "OTHERVALUE") ] in

  let updated =
    Shark.Run_block.process_single_command_execution es env_override leaf []
      null_runner raw_command
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
  let command = Shark.Command.of_string raw_command in
  let leaf = Shark.Leaf.v 42 (Option.get command) Shark.Leaf.Command [] [] in

  let es = Shark.Run_block.ExecutionState.init "id" "/some/path" [] in

  let runner_called = ref false in
  let runner _es _l _s _b =
    runner_called := true;
    Ok "otherid"
  in

  let updated =
    Shark.Run_block.process_single_command_execution es [] leaf [] runner
      raw_command
  in
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

let tests =
  [
    ("Test create initial state", `Quick, test_initial_block);
    ("Test simple change dir", `Quick, test_simple_change_dir);
    ("Test simple env update", `Quick, test_simple_env_udpate);
    ( "Test env update with command line override",
      `Quick,
      test_override_env_udpate );
    ("Test a simple command exection", `Quick, test_simple_command_execute);
  ]
