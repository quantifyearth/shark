open Brr
open Brr_io

let default_sharkdown = Jstr.v "# TODO"

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

let set_dot_img img_data =
  let img = El.to_jv @@ Edit.get_el_by_id "dot-img" in
  Jv.set img "src" (Jv.of_jstr img_data)

let setup_dot_button view =
  let dot_button = Edit.get_el_by_id "dot-btn" in
  let render _ =
    let fut =
      let open Fut.Result_syntax in
      let request =
        let editor = Code_mirror.Editor.View.state view in
        Console.log [ Edit.document editor ];
        let init =
          Fetch.Request.init ~method':(Jstr.v "PUT")
            ~body:(Fetch.Body.of_jstr (Edit.document editor))
            ()
        in
        Fetch.Request.v ~init (Jstr.v "/editor/dot")
      in
      let* response = Fetch.request request in
      let+ body = Fetch.Response.as_body response |> Fetch.Body.text in
      let png = Jstr.v "data:image/svg+xml;base64," in
      set_dot_img (Jstr.concat [ png; body ])
    in
    let _ : _ Fut.or_error = fut in
    ()
  in
  let _ : Ev.listener = Ev.listen Ev.click render (El.as_target dot_button) in
  ()

let () =
  get_markdown_file @@ fun v ->
  let view =
    match v with
    | Ok doc -> Edit.init ~doc ~exts:[||] ()
    | Error _ -> Edit.init ~doc:default_sharkdown ~exts:[||] ()
  in
  setup_dot_button view;
  match Document.find_el_by_id G.document (Jstr.v "reset") with
  | Some el ->
      let _listener : Ev.listener =
        Ev.listen Ev.click (fun _ -> reset view ()) (El.as_target el)
      in
      ()
  | _ -> ()
