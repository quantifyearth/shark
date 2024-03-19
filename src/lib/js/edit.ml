open Code_mirror
open Brr

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let markdown () =
  let md = Jv.get Jv.global "__CM__markdown" in
  Jv.apply md [||] |> Extension.of_jv

let get_el_by_id i =
  Brr.Document.find_el_by_id G.document (Jstr.of_string i) |> Option.get

let document e =
  Editor.State.doc e |> Text.to_jstr_array |> Array.to_list
  |> Jstr.concat ~sep:(Jstr.v "\n")

let init ?doc ?(exts = [||]) () =
  let open Editor in
  let config =
    State.Config.create ?doc
      ~extensions:(Array.concat [ [| basic_setup; markdown () |]; exts ])
      ()
  in
  let state = State.create ~config () in
  let opts = View.opts ~state ~parent:(get_el_by_id "editor") () in
  let view : View.t = View.create ~opts () in
  view

let set view ~doc ~exts =
  let open Editor in
  let config =
    State.Config.create ~doc
      ~extensions:(Array.concat [ [| basic_setup; markdown () |]; exts ])
      ()
  in
  let state = State.create ~config () in
  View.set_state view state
