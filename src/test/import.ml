let msg : [ `Msg of string ] Alcotest.testable =
  Alcotest.of_pp (fun ppf (`Msg m) -> Fmt.pf ppf "msg: %s" m)
