open Shark_ast

let test_leaf_basics () =
  let command = Command.of_string "test --i /data/arg1 --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  Alcotest.(check int) "Check id" 42 (Leaf.id leaf)

let test_leaf_command_sub_empty () =
  let command = Command.of_string "test --i /data/arg1 --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let test = Leaf.to_string_for_inputs leaf [] in
  let expected = [ "test --i /data/arg1 --o /data/arg2" ] in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_command_sub_simple () =
  let command = Command.of_string "test --i /data/arg1 --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist =
    [ ("/data/arg1", [ "/some/path/1" ]); ("/data/arg2", [ "/some/path/2" ]) ]
  in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected = [ "test --i /some/path/1 --o /some/path/2" ] in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_sub_simplewildcard () =
  let command = Command.of_string "test --i /data/arg1/* --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1/*") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist =
    [ ("/data/arg1/", [ "/some/path/1" ]); ("/data/arg2", [ "/some/path/2" ]) ]
  in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected = [ "test --i /some/path/1 --o /some/path/2" ] in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_sub_simplewildcard_multi () =
  let command = Command.of_string "test --i /data/arg1/* --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1/*") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist = [ ("/data/arg1/", [ "/some/path/1"; "/some/path/2" ]) ] in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected =
    [
      "test --i /some/path/1 --o /data/arg2";
      "test --i /some/path/2 --o /data/arg2";
    ]
  in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_sub_simplewildcard_generate () =
  let command = Command.of_string "test --i /data/arg1/* --o /data/arg2/+" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1/*") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist = [ ("/data/arg1/", [ "/some/path/1"; "/some/path/2" ]) ] in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected =
    [
      "test --i /some/path/1 --o /data/arg2/1";
      "test --i /some/path/2 --o /data/arg2/2";
    ]
  in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_command_sub_one_multi () =
  let command = Command.of_string "test --i /data/arg1 --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist =
    [
      ("/data/arg1", [ "/some/path/1"; "/other/path/1" ]);
      ("/data/arg2", [ "/some/path/2" ]);
    ]
  in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected =
    [
      "test --i /some/path/1 --o /some/path/2";
      "test --i /other/path/1 --o /some/path/2";
    ]
  in

  Alcotest.(check (list string)) "Simple sub" expected test

let test_leaf_command_sub_multi_multi () =
  let command = Command.of_string "test --i /data/arg1 --o /data/arg2" in
  let inputs = [ Datafile.v 0 (Fpath.v "/data/arg1") ]
  and outputs = [ Datafile.v 1 (Fpath.v "/data/arg2") ] in
  let leaf = Leaf.v 42 (Option.get command) Leaf.Command inputs outputs in

  let sublist =
    [
      ("/data/arg1", [ "/some/path/1"; "/other/path/1" ]);
      ("/data/arg2", [ "/some/path/2"; "/other/path/2" ]);
    ]
  in

  let test = Leaf.to_string_for_inputs leaf sublist in
  let expected =
    [
      "test --i /some/path/1 --o /some/path/2";
      "test --i /some/path/1 --o /other/path/2";
      "test --i /other/path/1 --o /some/path/2";
      "test --i /other/path/1 --o /other/path/2";
    ]
  in

  Alcotest.(check (list string)) "Simple sub" expected test

let tests =
  [
    ("Basic leaf test", `Quick, test_leaf_basics);
    ("Basic leaf simple wildcard", `Quick, test_leaf_sub_simplewildcard);
    ("Basic leaf multi wildcard", `Quick, test_leaf_sub_simplewildcard_multi);
    ("Generate target names", `Quick, test_leaf_sub_simplewildcard_generate);
    ("Basic leaf sub empty", `Quick, test_leaf_command_sub_empty);
    ("Basic leaf sub simple", `Quick, test_leaf_command_sub_simple);
    ("Basic leaf sub one map", `Quick, test_leaf_command_sub_one_multi);
    ("Basic leaf sub multiple map", `Quick, test_leaf_command_sub_multi_multi);
  ]
