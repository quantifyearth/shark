open Import

let frontmatter = Alcotest.of_pp Shark.Frontmatter.pp

let test_empty () =
  let testcase = "" in
  let res = Shark.Frontmatter.of_string testcase in
  Alcotest.(check (result frontmatter msg))
    "Empty expected" (Ok Shark.Frontmatter.empty) res

let test_parse_inputs () =
  let testcase =
    {|
inputs:
  p1: /data/stuff
  p2: /data/other/
  p3: /data/file.txt
  |}
  in
  let res = Shark.Frontmatter.of_string testcase in
  let expected =
    Shark.Frontmatter.v []
      [
        ("p1", Fpath.v "/data/stuff");
        ("p2", Fpath.v "/data/other/");
        ("p3", Fpath.v "/data/file.txt");
      ]
  in
  Alcotest.(check (result frontmatter msg)) "Empty expected" (Ok expected) res

let test_inputs_api () =
  let testcase =
    Shark.Frontmatter.v []
      [
        ("p1", Fpath.v "/data/stuff");
        ("p2", Fpath.v "/data/other/");
        ("p3", Fpath.v "/data/file.txt");
      ]
  in
  let res = List.map Fpath.to_string (Shark.Frontmatter.inputs testcase) in
  let expected = [ "/data/stuff"; "/data/other/"; "/data/file.txt" ] in
  Alcotest.(check (list string)) "Empty expected" expected res

let tests =
  [
    ("empty front matter", `Quick, test_empty);
    ("simple input list", `Quick, test_parse_inputs);
    ("simple input api", `Quick, test_inputs_api);
  ]
