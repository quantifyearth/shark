open Htmlit

let css =
  {|
  * {
  box-sizing: border-box;
}

/* Create two equal columns that floats next to each other */
.column {
  float: left;
  width: 50%;
  padding: 10px;
  height: 300px; /* Should be removed. Only for demonstration */
}

/* Clear floats after the columns */
.row:after {
  content: "";
  display: table;
  clear: both;
}

.btn {
  cursor: pointer;
  outline: 0;
  color: #fff;
  background-color: #0d6efd;
  border-color: #0d6efd;
  display: inline-block;
  font-weight: 400;
  line-height: 1.2;
  text-align: center;
  border: 1px solid transparent;
  padding: 6px 12px;
  font-size: 12px;
  border-radius: .25rem;
  transition: color .15s ease-in-out,background-color .15s ease-in-out,border-color .15s ease-in-out,box-shadow .15s ease-in-out;
  :hover {
      color: #fff;
      background-color: #0b5ed7;
      border-color: #0a58ca;
  }
}

.cm-editor {
  background: #eeffdd;
  max-height: 85vh;
}

|}

let body =
  El.splice
    [
      El.h3
        ~at:[ At.style "font-family:sans-serif" ]
        [ El.txt "Sharkdown Editor" ];
      El.div
        ~at:[ At.class' "row" ]
        [
          El.div
            ~at:[ At.class' "column" ]
            [
              El.p
                [
                  El.button
                    ~at:[ At.class' "btn"; At.id "dot-btn" ]
                    [ El.txt "Generate Dot" ];
                ];
              El.div ~at:[ At.id "editor" ] [];
            ];
          El.div
            ~at:
              [
                At.class' "column";
                At.style "height:90vh;overflow:auto;border:thin solid black";
              ]
            [ El.img ~at:[ At.id "dot-img" ] () ];
        ];
      El.script [ El.unsafe_raw Js.editor_js ];
    ]

let html = Html.html ~title:"Sharkdown Editor" ~css (`Html body)
