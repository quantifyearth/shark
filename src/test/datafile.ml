open Shark_ast

let test_basic_file_path () =
  let testcase = Fpath.v "/data/test/example.tif" in
  let test = Datafile.v 42 testcase in
  Alcotest.(check int) "Same id" 42 (Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.path test));
  Alcotest.(check string)
    "Same full path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.fullpath test));
  Alcotest.(check (option string)) "No subpath" None (Datafile.subpath test);
  Alcotest.(check bool) "Isn't wildcard" false (Datafile.is_wildcard test);
  Alcotest.(check bool) "Isn't dir" false (Datafile.is_dir test)

let test_sub_path () =
  let testcase = Fpath.v "/data/test/" in
  let test = Datafile.v ~subpath:"example.tif" 42 testcase in
  Alcotest.(check int) "Same id" 42 (Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.path test));
  Alcotest.(check string)
    "Same full path" "/data/test/example.tif"
    (Fpath.to_string (Datafile.fullpath test));
  Alcotest.(check (option string))
    "No subpath" (Some "example.tif") (Datafile.subpath test);
  Alcotest.(check bool) "Isn't wildcard" false (Datafile.is_wildcard test);
  Alcotest.(check bool) "Is dir" true (Datafile.is_dir test)

let test_basic_dir_with_wildcard () =
  let testcase = Fpath.v "/data/test/" in
  let test = Datafile.v ~subpath:"*" 42 testcase in
  Alcotest.(check int) "Same id" 42 (Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.path test));
  Alcotest.(check string)
    "Same full path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.fullpath test));
  Alcotest.(check (option string)) "No subpath" None (Datafile.subpath test);
  Alcotest.(check bool) "Is wildcard" true (Datafile.is_wildcard test);
  Alcotest.(check bool) "Is dir" true (Datafile.is_dir test)

let test_subpath_dir_with_wildcard () =
  let testcase = Fpath.v "/data/test/" in
  let test = Datafile.v ~subpath:"subpath/*" 42 testcase in
  Alcotest.(check int) "Same id" 42 (Datafile.id test);
  Alcotest.(check string)
    "Same path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.path test));
  Alcotest.(check string)
    "Same full path" (Fpath.to_string testcase)
    (Fpath.to_string (Datafile.fullpath test));
  Alcotest.(check (option string))
    "No subpath" (Some "subpath") (Datafile.subpath test);
  Alcotest.(check bool) "Is wildcard" true (Datafile.is_wildcard test);
  Alcotest.(check bool) "Is dir" true (Datafile.is_dir test)

let tests =
  [
    ("Basic file", `Quick, test_basic_file_path);
    ("Basic file with subpath", `Quick, test_sub_path);
    ("Canonical dir with wildcard", `Quick, test_basic_dir_with_wildcard);
    ("Dir with subpath with wildcard", `Quick, test_basic_dir_with_wildcard);
  ]
