open Brr
open Brr_io

(* let line_to_positions doc n =
   let line = Text.line n doc in
   (Text.Line.from line, Text.Line.to_ line) *)

(* let linter =
   let f view =
     let doc = Editor.View.state view |> Editor.State.doc in
     let lines =
       Text.to_jstr_array doc |> Array.to_list |> List.map Jstr.to_string
     in
     let result =
       Okra.Lint.lint_string_list ~include_sections:[ "Last Week" ] lines
     in
     let errs =
       match result with
       | Error (Format_error errs) ->
           let make_err (n, err) = (line_to_positions doc n, err) in
           List.map make_err errs
       | Error err -> [ ((0, Text.length doc), Okra.Lint.string_of_error err) ]
       | Ok () -> []
     in
     let diagnostic ~from ~to_ message =
       Lint.Diagnostic.create ~source:"okra-engineer" ~from ~to_ ~severity:Error
         ~message ()
     in
     let s = Storage.local G.window in
     let (_ : (unit, _) result) =
       Storage.set_item s storage_id (Jstr.v @@ String.concat "\n" lines)
     in
     Fut.return
       (List.map (fun ((from, to_), msg) -> diagnostic ~from ~to_ msg) errs
       |> Array.of_list)
   in
   Lint.create f *)

let default_sharkdown = Jstr.v "TODO"

let reset view () =
  let s = Storage.local G.window in
  Storage.clear s;
  Edit.set view ~doc:default_sharkdown ~exts:[||]

let get_markdown_file =
  let txt =
    let open Fut.Result_syntax in
    let* response = Fetch.url (Jstr.v "/editor/file") in
    Fetch.Response.as_body response |> Fetch.Body.text
  in
  Fut.await txt

let () =
  get_markdown_file @@ fun v ->
  let view =
    match v with
    | Ok doc -> Edit.init ~doc ~exts:[||] ()
    | Error _ -> Edit.init ~doc:default_sharkdown ~exts:[||] ()
  in
  match Document.find_el_by_id G.document (Jstr.v "reset") with
  | Some el ->
      let _listener : Ev.listener =
        Ev.listen Ev.click (fun _ -> reset view ()) (El.as_target el)
      in
      ()
  | _ -> ()
