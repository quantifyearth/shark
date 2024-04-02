let frontmatter = Alcotest.of_pp Shark.Frontmatter.pp

let test_empty () =
  let testcase = "" in
  match Shark.Frontmatter.of_string testcase with
  | Error e ->
      Alcotest.fail (Printf.sprintf "Failed %s" (match e with `Msg x -> x))
  | Ok fm ->
      Alcotest.(check frontmatter) "Empty expected" Shark.Frontmatter.empty fm

let test_inputs () =
  let testcase =
    {|
inputs:
- /data/stuff
- /data/other/
- /data/file.txt
  |}
  in
  match Shark.Frontmatter.of_string testcase with
  | Error e ->
      Alcotest.fail (Printf.sprintf "Failed %s" (match e with `Msg x -> x))
  | Ok fm ->
      let expected =
        Shark.Frontmatter.v
          [ ("inputs", [ "/data/stuff"; "/data/other/"; "/data/file.txt" ]) ]
          [
            Fpath.v "/data/stuff";
            Fpath.v "/data/other/";
            Fpath.v "/data/file.txt";
          ]
      in
      Alcotest.(check frontmatter) "Empty expected" expected fm

let tests =
  [
    ("empty front matter", `Quick, test_empty);
    ("simple input list", `Quick, test_inputs);
  ]
