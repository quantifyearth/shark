open Import
open Shark_ast

let frontmatter = Alcotest.of_pp Metadata.pp
let of_string s = Yaml.of_string s |> fun v -> Result.bind v Metadata.of_yaml

let test_empty () =
  let testcase = "" in
  let res = of_string testcase in
  Alcotest.(check (result frontmatter msg))
    "Empty expected" (Ok Metadata.empty) res

let test_parse_inputs () =
  let testcase =
    {|
inputs:
  p1: /data/stuff
  p2: /data/other/
  p3: /data/file.txt
  |}
  in
  let res = of_string testcase in
  let expected =
    Metadata.v []
      [
        ("p1", Fpath.v "/data/stuff");
        ("p2", Fpath.v "/data/other/");
        ("p3", Fpath.v "/data/file.txt");
      ]
  in
  Alcotest.(check (result frontmatter msg)) "Empty expected" (Ok expected) res

let test_inputs_api () =
  let testcase =
    Metadata.v []
      [
        ("p1", Fpath.v "/data/stuff");
        ("p2", Fpath.v "/data/other/");
        ("p3", Fpath.v "/data/file.txt");
      ]
  in
  let res = List.map Fpath.to_string (Metadata.inputs testcase) in
  let expected = [ "/data/stuff"; "/data/other/"; "/data/file.txt" ] in
  Alcotest.(check (list string)) "Empty expected" expected res

let tests =
  [
    ("empty front matter", `Quick, test_empty);
    ("simple input list", `Quick, test_parse_inputs);
    ("simple input api", `Quick, test_inputs_api);
  ]
