let block = Alcotest.of_pp Shark.Block.pp

let test_shark_build_block_no_hash () =
  let build_string_no_hash = "shark-build:gdal-env" in
  let expect = Shark.Block.build_or_run ~alias:"gdal-env" ~body:"" `Build in
  let test = Shark.Block.of_info_string ~body:"" build_string_no_hash in
  Alcotest.(check (option block)) "same block" (Some expect) test

let test_shark_build_with_hash_block () =
  let build_string_hash = "shark-build:gdal-env:abcdefg" in
  let expect =
    Shark.Block.build_or_run ~hash:"abcdefg" ~alias:"gdal-env" ~body:"" `Build
  in
  let test = Shark.Block.of_info_string ~body:"" build_string_hash in
  Alcotest.(check (option block)) "same block hash" (Some expect) test

let test_shark_empty_block () =
  let ocaml = "ocaml" in
  let test = Shark.Block.of_info_string ~body:"" ocaml in
  Alcotest.(check (option block)) "same default block" None test

let test_shark_run_block_no_hash () =
  let info = "shark-run:somecontainer" in
  let body = "$ python3 something.py\n$ python3 other.py" in
  let expect = Shark.Block.build_or_run ~alias:"somecontainer" ~body `Run in
  let test = Shark.Block.of_info_string ~body info in
  Alcotest.(check (option block)) "same block" (Some expect) test

let test_shark_run_multiple_commands () =
  let body = "$ python3 something.py\n$ python3 other.py" in
  let block = Shark.Block.build_or_run ~alias:"somecontainer" ~body `Run in
  let expect = [ "$ python3 something.py"; "$ python3 other.py" ] in
  let test = Shark.Block.command_list block in
  Alcotest.(check (list string)) "Some commands" expect test

let test_shark_run_multiline_command () =
  let body = "$ python3 something.py\\\n\targ1 arg2" in
  let block = Shark.Block.build_or_run ~alias:"somecontainer" ~body `Run in
  let expect = [ "$ python3 something.py arg1 arg2" ] in
  let test = Shark.Block.command_list block in
  Alcotest.(check (list string)) "Single command" expect test

let test_git_import_block () =
  let body =
    "https://example.com/quantifyearth/littlejohn.git /data/littlejohn"
  in
  let block = Shark.Block.import body in
  let expected =
    [ ("https://example.com/quantifyearth/littlejohn.git", "/data/littlejohn") ]
  in
  let test =
    List.map
      (fun (u, p) -> (Uri.to_string u, Fpath.to_string p))
      (Shark.Block.imports block)
  in
  Alcotest.(check (list (pair string string))) "Single import" expected test;

  let spec, src_dir = Shark.Block.import_spec block in
  let specbody = Sexplib.Sexp.to_string_hum (Obuilder_spec.sexp_of_t spec) in
  Alcotest.(check bool)
    "Found git command" true
    (Astring.String.is_infix ~affix:"git clone" specbody);
  Alcotest.(check (option string)) "No src_dir change" None src_dir

let test_http_import_block () =
  let body = "https://example.com/data/something.csv /data/src.csv" in
  let block = Shark.Block.import body in
  let expected =
    [ ("https://example.com/data/something.csv", "/data/src.csv") ]
  in
  let test =
    List.map
      (fun (u, p) -> (Uri.to_string u, Fpath.to_string p))
      (Shark.Block.imports block)
  in
  Alcotest.(check (list (pair string string))) "Single import" expected test;

  let spec, src_dir = Shark.Block.import_spec block in
  let specbody = Sexplib.Sexp.to_string_hum (Obuilder_spec.sexp_of_t spec) in
  Alcotest.(check bool)
    "Found git command" true
    (Astring.String.is_infix ~affix:"curl -O" specbody);
  Alcotest.(check (option string)) "No src_dir change" None src_dir

let test_file_import_block_no_schema () =
  let body = "/home/michael/file.csv /data/file.csv" in
  let block = Shark.Block.import body in
  let expected = [ ("/home/michael/file.csv", "/data/file.csv") ] in
  let test =
    List.map
      (fun (u, p) -> (Uri.to_string u, Fpath.to_string p))
      (Shark.Block.imports block)
  in
  Alcotest.(check (list (pair string string))) "Single import" expected test;

  let spec, src_dir = Shark.Block.import_spec block in
  let specbody = Sexplib.Sexp.to_string_hum (Obuilder_spec.sexp_of_t spec) in
  Alcotest.(check bool)
    "Found git command" true
    (Astring.String.is_infix ~affix:"copy" specbody);
  Alcotest.(check (option string)) "Src_dir change" (Some "/home/michael") src_dir

let test_file_import_block_with_schema () =
  let body = "file:///home/michael/file.csv /data/file.csv" in
  let block = Shark.Block.import body in
  let expected = [ ("file:///home/michael/file.csv", "/data/file.csv") ] in
  let test =
    List.map
      (fun (u, p) -> (Uri.to_string u, Fpath.to_string p))
      (Shark.Block.imports block)
  in
  Alcotest.(check (list (pair string string))) "Single import" expected test;

  let spec, src_dir = Shark.Block.import_spec block in
  let specbody = Sexplib.Sexp.to_string_hum (Obuilder_spec.sexp_of_t spec) in
  Alcotest.(check bool)
    "Found git command" true
    (Astring.String.is_infix ~affix:"copy" specbody);
    Alcotest.(check (option string)) "Src_dir change" (Some "/home/michael") src_dir

let tests =
  [
    ("shark build block, no hash", `Quick, test_shark_build_block_no_hash);
    ("shark build block, with hash", `Quick, test_shark_build_with_hash_block);
    ("shark empty block", `Quick, test_shark_empty_block);
    ("shark run block", `Quick, test_shark_run_block_no_hash);
    ("parsing multiple commands", `Quick, test_shark_run_multiple_commands);
    ("parsing multiline command", `Quick, test_shark_run_multiline_command);
    ("parsing basic git import", `Quick, test_git_import_block);
    ("parsing basic http import", `Quick, test_http_import_block);
    ( "parsing basic file import no schema",
      `Quick,
      test_file_import_block_no_schema );
    ( "parsing basic file import with schema",
      `Quick,
      test_file_import_block_with_schema );
  ]
