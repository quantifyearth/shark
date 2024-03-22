open Htmlit

let file =
  El.unsafe_raw
    {| <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 0 0-2 2v16c0 1.1.9 2 2 2h12a2 2 0 0 0 2-2V8l-6-6z"/><path d="M14 3v5h5M16 13H8M16 17H8M10 9H8"/></svg> |}

let code =
  El.unsafe_raw
    {|<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></svg>|}

let log =
  El.unsafe_raw
    {|<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17 9.5H3M21 4.5H3M21 14.5H3M17 19.5H3"/></svg>|}

let built_in_css =
  {|    *, *::before, *::after { box-sizing: border-box }
    body { min-height: 100vh; min-height: 100svh; }
    body, h1, h2, h3, h4, p, figure, blockquote, dl, dd { margin: 0; }
    pre, input, button, textarea, select { font: inherit }

    :root
    {  font-size: 100%;
       /* font-synthesis: none; */
       -webkit-text-size-adjust: none;

      --font_headings: system-ui, sans-serif;
      --font_body: system-ui, sans-serif;
      --font_mono: monospace;

      --font_m: 1rem; --leading_m: 1.5rem;
      --font_s: 0.82rem;
      --font_l: 1.125rem; --leadig_l: 1.34rem;
      --font_xl: 1.5rem; --leading_xl: 1.8rem;
      --font_xxl: 2.5rem; --leading_xxl: 3rem;

      --font_mono_ratio:
        /* mono / body size, difficult to find a good cross-browser value */
           0.92;
      --leading_mono_m: calc(var(--leading_m) * var(--font_mono_ratio));

      --sp_xxs: calc(0.25 * var(--leading_m));
      --sp_xs: calc(0.5 * var(--leading_m));
      --sp_s: calc(0.75 * var(--leading_m));
      --sp_m: var(--leading_m);
      --sp_l: calc(1.125 * var(--leading_m));
      --sp_xl: calc(1.5 * var(--leading_m));
      --sp_xxl: calc(2.0 * var(--leading_m));

      --measure_m: 73ch;
      --page_inline_pad: var(--sp_m);
      --page_block_pad: var(--sp_xl);

      --blockquote_border: 2px solid #ACACAC;
      --rule_border: 1px solid #CACBCE;
      --heading_border: 1px solid #EAECEF;
      --table_cell_pad: 0.4em;
      --table_hover: #f5f5f5;
      --table_sep: #efefef;
      --table_cell_inline_pad: 0.625em;
      --table_cell_block_pad: 0.25em;

      --code_span_bg: #EFF1F3;
      --code_span_inline_pad: 0.35ch;
      --code_block_bg: #F6F8FA;
      --code_block_bleed: 0.8ch;
      --code_block_block_pad: 1ch;

      --a_fg: #0969DA;
      --a_fg_hover: #1882ff;
      --a_visited: #8E34A5;
      --target_color: #FFFF96;
    }

    body
    { font-family: var(--font_body); font-weight: 400;
      font-size: var(--font_m); line-height: var(--leading_m);
      max-inline-size: var(--measure_m);
      padding-block: var(--page_block_pad);
      padding-inline: var(--page_inline_pad);
      margin-inline: auto;
      background-color: white; color: black; }

    body > *:first-child { margin-block-start: 0 }
    body * + * { margin-block-start: var(--sp_xs) }

    /* Blocks */

    h1, h2, h3, h4, h5, h6
    { font-family: var(--font_headings); font-weight: 600}

    h1 { font-size: var(--font_xxl); line-height: var(--leading_xxl);
         margin-block-start: var(--sp_xl); }

    h3 + *, h4 + *, h5 + *, h6 + *
    { margin-block-start: var(--sp_xs); }

    h2 { font-size: var(--font_xl); line-height: var(--leading_xl);
         margin-block-start: var(--sp_m);
         padding-block-end: var(--sp_xxs);
         border-bottom: var(--heading_border); }

    h3 { font-size: var(--font_l); line-height: var(--leading_l);
         margin-block-start: var(--sp_m); }

    h4 { font-weight: 400; font-style: oblique; }

    ul, ol { padding-inline-start: 3ch; }
    li + li { margin-block-start: var(--sp_xxs); }

    li > .task { display: flex; margin:0; padding:0; align-items: baseline;
                 column-gap: var(--sp_xxs); }
    li > .task > input { padding:0; margin:0 }
    li > .task > div { margin:0; padding:0 }

    blockquote > blockquote { margin-inline: 0.25ch; }
    blockquote
    {  margin-inline: 2ch;
       padding-inline: 1ch;
       border-left: var(--blockquote_border) }

    hr + * { margin-block-start: calc(var(--sp_s) - 1px); }
    hr { border: 0; border-block-end: var(--rule_border);
         width: 10ch;
         margin-block-start: var(--sp_s); margin-inline: auto; }

    pre
    { line-height: var(--leading_mono_m);
      white-space: pre-wrap;
      overflow-wrap: break-word;
      background-color: var(--code_block_bg);
      padding-block: var(--code_block_block_pad);
      padding-inline: var(--code_block_bleed);
      margin-inline: calc(-1.0 * var(--code_block_bleed)) }

    pre code { padding-inline: 0; background-color: inherit }

    [role="region"] { overflow: auto }
    table { border-collapse: separate; border-spacing: 0; white-space: nowrap }
    tr:hover > td { background: var(--table_hover) }
    th, td, th.left, td.left { text-align: left }
    th.right, td.right { text-align: right }
    th.center, td.center { text-align: center }
    td, th { border: 0px solid var(--table_sep); border-block-end-width: 1px }
    tr:first-child td { border-block-start-width: 1px; } /* headerless */
    th { font-weight: 600 }
    th, td { padding-inline: var(--table_cell_inline_pad);
             padding-block: var(--table_cell_block_pad); }

    /* Inlines */

    code
    { font-family: var(--font_mono);
      font-size: calc(1em * var(--font_mono_ratio));
      background-color: var(--code_span_bg);
      padding-inline: var(--code_span_inline_pad);
      border-radius: 3px;
      white-space: break-spaces; }

    a:hover { color: var(--a_fg_hover) }
    a:hover:visited { color: var(--a_visited); }
    a { color: var(--a_fg);
        text-decoration: underline;
        text-decoration-thickness: 0.04em;
        text-decoration-skip-ink: all;
        text-underline-offset: 3px; }

    *:hover > a.anchor { visibility: visible; }
    body > *:hover:first-child > a.anchor { visibility: hidden }
    a.anchor:visited { color: var(--a_fg); }
    a.anchor:before { content: "#";  }
    a.anchor:hover { color: var(--a_fg_hover); }
    a.anchor
    { visibility: hidden; position: absolute;
      font-weight: 400; font-style: normal;
      font-size: 0.9em;
      margin-left: -2.5ch;
      padding-right: 1ch; padding-left: 1ch; /* To remain selectable */
      color: var(--a_fg_hover);
      text-decoration: none; }

    *:target
    { background-color: var(--target_color);
      box-shadow: 0 0 0 3px var(--target_color); }

    em { font-style: oblique }
    b, strong { font-weight: 600 }
    small { font-size: var(--font_s) }
    sub, sup { vertical-align: baseline;
               font-size: 0.75em;
               line-height: 0; position:relative }
    sub { bottom: -0.25em }
    sup { top: -0.5em }

    /* Footnotes */

    a.fn-label { text-decoration: none; }
    a:target.fn-label { box-shadow: none }

    [role="doc-endnotes"]
    { font-size: 87.5%;
      line-height: calc(0.875 * var(--leading_m));
      margin-block-start: var(--sp_m);
      border-block-start: var(--rule_border); }
    [role="doc-endnotes"] > ol > li * + * { margin-block-start: var(--sp_xxs) }
    [role="doc-endnotes"] > ol { padding-inline-start: 2ex; }
    [role="doc-endnotes"] a.fn-label { padding-right:0.5ex; }

    [role="doc-endnotes"] > ol > li:target
    { background-color: inherit; box-shadow: none }
    [role="doc-endnotes"] > ol > li:target::marker
    { font-weight:900; /* Can't set background */ }
|}

let html_block html =
  let s = El.to_string ~doctype:false html in
  Cmarkit.Block.Html_block
    (Cmarkit.Block_line.list_of_string s, Cmarkit.Meta.none)

let custom_document_renderer _ = function
  | Cmarkit.Block.Code_block (node, _) as v ->
      (* We're only interested in getting the hash here. *)
      let info =
        Cmarkit.Block.Code_block.info_string node |> fun v ->
        Option.bind v (fun (v, _) -> Block.of_info_string ~body:"" v)
      in
      let info_block =
        match Option.bind info (fun v -> Block.hash v) with
        | None ->
            Cmarkit.Block.Thematic_break
              (Cmarkit.Block.Thematic_break.make (), Cmarkit.Meta.none)
        | Some hash ->
            html_block
              (El.div
                 ~at:[ At.style "font-family:monospace;font-size:medium" ]
                 [
                   El.a
                     ~at:
                       [
                         At.v "target" "_blank";
                         At.href (Fmt.str "/logs/%s" hash);
                         At.style "text-decoration:none;";
                       ]
                     [
                       El.button
                         ~at:
                           [
                             At.style
                               "display:inline-flex;align-items:center;padding:0.2em \
                                0.4em";
                           ]
                         [ log; El.nbsp; El.txt (Fmt.str "Logs") ];
                     ];
                   El.a
                     ~at:
                       [
                         At.v "target" "_blank";
                         At.href (Fmt.str "/files/%s" hash);
                         At.style "text-decoration:none;";
                       ]
                     [
                       El.button
                         ~at:
                           [
                             At.style
                               "display:inline-flex;align-items:center;padding:0.2em \
                                0.4em;margin-left: 1em";
                           ]
                         [ file; El.nbsp; El.txt (Fmt.str "Files") ];
                     ];
                   El.a
                     ~at:
                       [
                         At.v "target" "_blank";
                         At.href (Fmt.str "/logs/%s" hash);
                         At.style "text-decoration:none;";
                       ]
                     [
                       El.button
                         ~at:
                           [
                             At.style
                               "display:inline-flex;align-items:center;padding:0.2em \
                                0.4em;margin-left: 1em";
                           ]
                         [ code; El.nbsp; El.txt (Fmt.str "Code") ];
                     ];
                 ])
      in
      `Map (Some (Cmarkit.Block.Blocks ([ info_block; v ], Cmarkit.Meta.none)))
  | _ -> `Default

let html_headers = Http.Header.of_list [ ("content-type", "text/html") ]
let text_headers = Http.Header.of_list [ ("content-type", "text/plain") ]

let respond_html html =
  Cohttp_eio.Server.respond ~status:`OK
    ~body:(Cohttp_eio.Body.of_string html)
    ~headers:html_headers ()

let respond_txt html =
  Cohttp_eio.Server.respond ~status:`OK
    ~body:(Cohttp_eio.Body.of_string html)
    ~headers:text_headers ()

let respond_not_found =
  Cohttp_eio.Server.respond ~status:`Not_found
    ~body:(Cohttp_eio.Body.of_string "Not Found!")
    ~headers:html_headers ()

let serve markdown_file =
  let mapper = Cmarkit.Mapper.make ~block:custom_document_renderer () in
  let md = Eio.Path.load markdown_file in
  let html =
    Cmarkit.Doc.of_string md
    |> Cmarkit.Mapper.map_doc mapper
    |> Cmarkit_html.of_doc ~safe:false
    |> fun s -> Html.html ~title:"shark md" ~css:built_in_css (`String s)
  in
  respond_html html

let serve_logs fs (Obuilder.Store_spec.Store ((module Store), store)) hash =
  let log_path = Lwt_eio.Promise.await_lwt @@ Store.log_file store hash in
  let log = Eio.Path.(load (fs / log_path)) in
  let log_code =
    El.div
      [
        El.h2 [ El.txt "Log file" ];
        El.p [ El.txt "The logs for build "; El.code [ El.txt hash ] ];
        El.pre [ El.code [ El.txt ("Log: " ^ log) ] ];
      ]
  in
  respond_html (Html.html ~title:"logs" ~css:built_in_css (`Html log_code))

type file = {
  name : string;
  date : string;
  size : Optint.Int63.t;
  is_directory : bool;
}

let pp_time f (sec, nsec) =
  let tm = Unix.localtime (Int64.to_float sec) in
  Format.fprintf f "%04d-%02d-%02d %02d:%02d:%02d.%09d +0000"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec nsec

let serve_files fs (Obuilder.Store_spec.Store ((module Store), store)) hash dir
    =
  match Lwt_eio.Promise.await_lwt @@ Store.result store hash with
  | None ->
      respond_html
        (Html.html ~title:"Files" ~css:built_in_css
           (`Html (El.p [ El.txt "No result!" ])))
  | Some path ->
      let get_files_from_directory directory =
        let file_names = Eio.Path.(read_dir directory) in
        let files =
          List.map
            (fun v -> (v, Eio.Path.stat ~follow:true Eio.Path.(directory / v)))
            file_names
        in
        let date (stat : Eio.File.Stat.t) =
          let secs = Int64.of_float stat.ctime in
          pp_time Format.str_formatter (secs, 0);
          Format.flush_str_formatter ()
        in
        List.map
          (fun (v, (stat : Eio.File.Stat.t)) ->
            let is_directory =
              match stat.kind with `Directory -> true | _ -> false
            in
            { name = v; date = date stat; size = stat.size; is_directory })
          files
      in
      let files =
        match dir with
        | None ->
            let dir = Eio.Path.(fs / path / "rootfs") in
            get_files_from_directory dir
        | Some dir ->
            let dir =
              Astring.String.trim
                ~drop:(function '/' -> true | _ -> false)
                dir
            in
            let directory = Eio.Path.(fs / path / "rootfs" / dir) in
            get_files_from_directory directory
      in
      let path_so_far =
        match dir with
        | None -> "/"
        | Some dir ->
            Astring.String.trim ~drop:(function '/' -> true | _ -> false) dir
      in
      let log_code =
        El.div
          [
            El.h2
              [
                El.txt "Files: ";
                El.code
                  [
                    El.txt
                      (if path_so_far = "/" then "/" else "/" ^ path_so_far);
                  ];
              ];
            El.ul
              (List.map
                 (fun v ->
                   El.li
                     [
                       (if v.is_directory then
                          El.a
                            ~at:
                              [
                                At.href
                                  (Fmt.str "/files/%s/%s/%s" hash path_so_far
                                     v.name);
                              ]
                            [ El.em [ El.txt v.name ] ]
                        else El.em [ El.txt v.name ]);
                       El.nbsp;
                       El.txt (Optint.Int63.to_string v.size);
                       El.nbsp;
                       El.txt v.date;
                     ])
                 files);
          ]
      in
      respond_html (Html.html ~title:"logs" ~css:built_in_css (`Html log_code))

let serve_editor = function
  | None -> respond_html Editor.html
  | Some md_file ->
      let document = Eio.Path.load md_file in
      respond_txt document

let run_dot proc dot =
  Eio.Process.parse_out proc
    ~stdin:(Eio.Flow.string_source dot)
    Eio.Buf_read.take_all [ "dot"; "-Tsvg" ]

let serve_dot proc _req body =
  let template_markdown = Eio.Flow.read_all body in
  let txt = Dotrenderer.render ~template_markdown in
  let png = run_dot proc txt |> Base64.encode_string in
  respond_txt png

let edit_routes ~proc md_file (_conn : Cohttp_eio.Server.conn) request body =
  let open Routes in
  [
    route (s "editor" /? nil) (serve_editor None);
    route (s "editor" / s "file" /? nil) (serve_editor (Some md_file));
    route (s "editor" / s "dot" /? nil) (serve_dot proc request body);
  ]

let router ~proc ~fs ~store md_file (conn : Cohttp_eio.Server.conn) request body
    =
  let open Routes in
  let store = Lwt_eio.Promise.await_lwt store in
  let routes =
    [
      route nil (serve md_file);
      route (s "logs" / str /? nil) (serve_logs fs store);
      route
        (s "files" / str /? nil)
        (fun hash -> serve_files fs store hash None);
      route
        (s "files" / str /? wildcard)
        (fun hash dir ->
          serve_files fs store hash (Some (Parts.wildcard_match dir)));
    ]
  in
  let routes = routes @ edit_routes ~proc md_file conn request body in
  let router = Routes.one_of routes in
  match Routes.match' router ~target:(Http.Request.resource request) with
  | FullMatch a -> a
  | MatchWithTrailingSlash a -> a
  | NoMatch -> respond_not_found
