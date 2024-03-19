open Htmlit

let body =
  El.div
    [
      El.div ~at:[ At.id "editor" ] []; El.script [ El.unsafe_raw Js.editor_js ];
    ]

let html =
  Html.html ~title:"Sharkdown Editor" ~css:"#editor { width: 50%; }"
    (`Html body)
