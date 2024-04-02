let test_basic_file_path () =
  let testcase = Fpath.v "/data/test/example.tif" in
  let test = Shark.Datafile.v 42 testcase in
  Alcotest.(check int) "Same id" 42 (Shark.Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Shark.Datafile.path test));
  Alcotest.(check (option string))
    "No subpath" None
    (Shark.Datafile.subpath test);
  Alcotest.(check bool) "Isn't wildcard" false (Shark.Datafile.is_wildcard test);
  Alcotest.(check bool) "Isn't dir" false (Shark.Datafile.is_dir test)

let test_basic_dir_with_wildcard () =
  let testcase = Fpath.v "/data/test/" in
  let test = Shark.Datafile.v ~subpath:"*" 42 testcase in
  Alcotest.(check int) "Same id" 42 (Shark.Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Shark.Datafile.path test));
  Alcotest.(check (option string))
    "No subpath" None
    (Shark.Datafile.subpath test);
  Alcotest.(check bool) "Is wildcard" true (Shark.Datafile.is_wildcard test);
  Alcotest.(check bool) "Is dir" true (Shark.Datafile.is_dir test)

let tests =
  [
    ("Basic file", `Quick, test_basic_file_path);
    ("Canonical dir with wildcard", `Quick, test_basic_dir_with_wildcard);
  ]
