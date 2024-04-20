let reporter clock ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_stamp h k ppf fmt =
      let dt = Mtime_clock.count clock |> Mtime.Span.to_uint64_ns in
      Fmt.kpf k ppf
        ("[%a] %a %a @[" ^^ fmt ^^ "@]@.")
        Fmt.uint64_ns_span dt Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags:_ fmt -> with_stamp header k ppf fmt
  in
  { Logs.report }
