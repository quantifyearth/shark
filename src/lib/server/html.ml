let html ~title ~css body =
  Fmt.str
    {|<!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s</title>
    <style>%s</style>
  </head>
  <body>
  %s</body>
  </html>|}
    title css
    (match body with
    | `String s -> s
    | `Html body -> Htmlit.El.to_string ~doctype:false body)
