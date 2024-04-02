open Import

let frontmatter = Alcotest.of_pp Shark.Frontmatter.pp

let test_empty () =
  let testcase = "" in
  let res = Shark.Frontmatter.of_string testcase in
  Alcotest.(check (result frontmatter msg))
    "Empty expected" (Ok Shark.Frontmatter.empty) res

let test_inputs () =
  let testcase =
    {|
inputs:
- /data/stuff
- /data/other/
- /data/file.txt
  |}
  in
  let res = Shark.Frontmatter.of_string testcase in
  let expected =
    Shark.Frontmatter.v
      [ ("inputs", [ "/data/stuff"; "/data/other/"; "/data/file.txt" ]) ]
      [
        Fpath.v "/data/stuff"; Fpath.v "/data/other/"; Fpath.v "/data/file.txt";
      ]
  in
  Alcotest.(check (result frontmatter msg)) "Empty expected" (Ok expected) res

let tests =
  [
    ("empty front matter", `Quick, test_empty);
    ("simple input list", `Quick, test_inputs);
  ]
