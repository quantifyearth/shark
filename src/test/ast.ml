let command = Alcotest.of_pp Shark.Ast.pp

let test_single_block () =
  let template_markdown =
    {|
```shark-run:containeralias
$ python3 something.py /data/something.txt
```
  |}
  in
  let test = Shark.Ast.of_sharkdown ~template_markdown in
  let groups = Shark.Ast.to_list test in
  Alcotest.(check int) "Single command group expected" 1 (List.length groups);
  let leaves = Shark.Commandgroup.children (List.nth groups 0) in
  Alcotest.(check int) "Single command expected" 1 (List.length leaves)

let test_multicommand_block () =
  let template_markdown =
    {|
```shark-run:containeralias
$ python3 something.py /data/something.txt
$ python3 else.py /data/something.txt
```
  |}
  in
  let test = Shark.Ast.of_sharkdown ~template_markdown in
  let groups = Shark.Ast.to_list test in
  Alcotest.(check int) "Single command group expected" 1 (List.length groups);
  let leaves = Shark.Commandgroup.children (List.nth groups 0) in
  Alcotest.(check int) "Single command expected" 2 (List.length leaves)

let tests =
  [
    ("Test simple single command block", `Quick, test_single_block);
    ("Test simple milticommand block", `Quick, test_multicommand_block);
  ]