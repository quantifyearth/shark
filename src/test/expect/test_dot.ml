let () =
  let tmf = In_channel.with_open_bin "./tmf.md" In_channel.input_all in
  let dot = Shark.Dotrenderer.render ~template_markdown:tmf in
  Fmt.(string stdout) dot
