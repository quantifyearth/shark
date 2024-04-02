let block = Alcotest.of_pp Shark.Block.pp

let test_shark_block () =
  let build_string_no_hash = "shark-build:gdal-env" in
  let expect = Shark.Block.v ~alias:"gdal-env" ~body:"" `Build in
  let test = Shark.Block.of_info_string ~body:"" build_string_no_hash in
  Alcotest.(check (option block)) "same block" (Some expect) test;

  let build_string_hash = "shark-build:gdal-env:abcdefg" in
  let expect =
    Shark.Block.v ~hash:"abcdefg" ~alias:"gdal-env" ~body:"" `Build
  in
  let test = Shark.Block.of_info_string ~body:"" build_string_hash in
  Alcotest.(check (option block)) "same block hash" (Some expect) test;

  let ocaml = "ocaml" in
  let test = Shark.Block.of_info_string ~body:"" ocaml in
  Alcotest.(check (option block)) "same default block" None test

let tests = [ ("shark blocks", `Quick, test_shark_block) ]
