let template title body =
  let open Htmlit in
  let more_head =
    El.splice
      [
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/base-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-responsive-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/buttons-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/tables-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css";
            ]
          ();
        El.script
          ~at:[ At.src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" ]
          [];
        El.style
          [
            El.unsafe_raw
              {|
        body {
            margin: 24px
        }

        .l-box {
            padding: 0em 2em;
        }

        .pure-g .pure-u-3-5 div {
            border-right: thin solid grey;
        }
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
        
    pre
    { line-height: var(--leading_mono_m);
      white-space: pre-wrap;
      overflow-wrap: break-word;
      background-color: var(--code_block_bg);
      padding-block: var(--code_block_block_pad);
      padding-inline: var(--code_block_bleed);
      margin-inline: calc(-1.0 * var(--code_block_bleed)) }

        #map { width: 100%; height: 800px }
    |};
          ];
      ]
  in
  El.page ~lang:"en" ~more_head ~title body

let divc class' children =
  Htmlit.El.div ~at:[ Htmlit.At.class' class' ] children

let pure_button ?(disabled = false) href txt =
  Htmlit.El.a
    ~at:
      [
        Htmlit.At.href href;
        (if disabled then Htmlit.At.disabled else Htmlit.At.void);
        Htmlit.At.class' "pure-button";
      ]
    [ Htmlit.El.txt txt ]

let map geojsons =
  let open Htmlit in
  let add =
    {|fetch(url).then(res => res.json()).then(data => {
      // add GeoJSON layer to the map once the file is loaded
      var k = L.geoJson(data, { 
        style: lineStyle, 
        minZoom: 3,
        pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
      }});
      if (i == 0) { map.setView(k.getBounds().getCenter(), 6) };
      var obj = {};
      // Extracting filepath from query params of URL
      var file = url.split("/")[url.split("/").length - 1]
      obj[file] = k;
      return obj;
    })|}
  in
  let jsarr =
    Fmt.str "var urls = [ %a ]"
      Fmt.(list ~sep:(Fmt.any ", ") (quote string))
      geojsons
  in
  El.script
    [
      El.unsafe_raw
        (Fmt.str
           {|
      var geojsonMarkerOptions = {
          radius: 3,
          fillColor: "#ff0000",
          color: "#000",
          weight: 1,
          opacity: 0.4,
          fillOpacity: 0.4
      };
      var lineStyle = {
          "color": "#ff7800",
          "weight": 2,
          "opacity": 0.3
      };
      var osm = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
          minZoom: 3,
          maxZoom: 19,
          attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      })
      var map = L.map('map', {
        center: [-5.006552150273841, -1.1807189565615772e-05],
        preferCanvas: true,
        zoom: 13,
        minZoom: 3,
        maxZoom: 13,
        layers: [ osm ]
      })
      %s
      var proms = urls.map((url, i) => %s)
      Promise.all(proms).then((layers) => { 
        var layers = layers.reduce(((r, c) => Object.assign(r, c)), {});
        console.log(layers);
        L.control.layers({ "OSM": osm }, layers, {collapsed: false}).addTo(map)
      })
    |}
           jsarr add);
    ]

module Table = struct
  type t = string list list

  let render t =
    let open Htmlit in
    let row ?(header = false) s =
      let el = if header then El.th else El.td in
      El.tr (List.map (fun s -> el [ El.txt s ]) s)
    in
    match t with
    | header :: data ->
        let header = El.thead [ row ~header:true header ] in
        El.table
          ~at:[ At.class' "pure-table" ]
          [ header; El.tbody (List.map (fun r -> row r) data) ]
    | _ -> El.txt "Something went wrong rendering the tabluar data"
end

let build ?inputs ?(geojsons = []) ?(jsons = []) ?(images = []) ?(tabular = [])
    ~title ~id ~manifest () =
  let open Htmlit in
  El.splice
    [
      divc "pure-g"
        [
          divc "pure-u-3-5"
            [
              divc "l-box"
                [
                  El.h2 [ El.txt "Data and Build Information" ];
                  El.em [ El.txt ("Job ID: " ^ id) ];
                  El.p
                    [
                      pure_button (Fmt.str "/download/%s" id) "Download Data";
                      pure_button ~disabled:true "#" "Run Shell";
                      pure_button ~disabled:true "#" "Run Notebook";
                    ];
                  El.p
                    [
                      El.txt
                        "The following files were generated during this build \
                         step. Clicking 'Download Data' will zip these up and \
                         begin a download for them. This is only really \
                         feasible on relatively small datasets.";
                    ];
                  El.pre
                    ~at:[ At.style "max-height:300px; overflow-y:scroll" ]
                    [ El.code [ El.txt manifest ] ];
                  (match geojsons with
                  | [] -> El.void
                  | _ ->
                      El.splice
                        [
                          El.h2 [ El.txt "Maps" ];
                          El.p
                            [
                              El.txt
                                "The map below plots the obviously plottable \
                                 pieces of data that we could find. For now \
                                 this is any GeoJSON data files.";
                            ];
                          El.p
                            [
                              El.em
                                [
                                  El.txt
                                    "Note we've zoomed into bounding box of \
                                     the first piece of data, the map may \
                                     contain others!";
                                ];
                            ];
                          El.div ~at:[ At.id "map" ] [];
                        ]);
                  (match images with
                  | [] -> El.void
                  | urls ->
                      El.splice
                        ([
                           El.h2 [ El.txt "Images" ];
                           El.p
                             [
                               El.txt
                                 "Images generated as output data, we only \
                                  render PNG or JPEG images for now as they \
                                  are likely to be small enough.";
                             ];
                         ]
                        @ List.map
                            (fun url ->
                              El.img ~at:[ At.v "width" "100%"; At.src url ] ())
                            urls));
                  (match jsons with
                  | [] -> El.void
                  | contents ->
                      El.splice
                        ([
                           El.h2 [ El.txt "JSON Files" ];
                           El.p
                             [
                               El.txt
                                 "Raw JSON files that are not geospatial in \
                                  nature.";
                             ];
                         ]
                        @ List.map
                            (fun content ->
                              El.pre [ El.code [ El.txt content ] ])
                            contents));
                  (match tabular with
                  | [] -> El.void
                  | datas ->
                      El.splice
                        [
                          El.h2 [ El.txt "Tabular Data" ];
                          El.p
                            [
                              El.txt
                                "Tables for any CSV files found in the output \
                                 data directory.";
                            ];
                          El.splice (List.map Table.render datas);
                        ]);
                ];
            ];
          divc "pure-u-2-5"
            [
              divc "l-box"
                [
                  El.h3 [ El.txt "Build Summary" ];
                  El.div
                    (match inputs with
                    | None -> []
                    | Some (i : Obuilder.S.run_input) ->
                        let base =
                          El.div
                            [
                              El.p [ El.txt "Base Image" ];
                              El.pre [ El.code [ El.txt i.base ] ];
                            ]
                        in
                        let cmd =
                          El.div
                            [
                              El.p [ El.txt "The command run was:" ];
                              El.pre [ El.code [ El.txt i.cmd ] ];
                            ]
                        in
                        let roms =
                          if i.rom = [] then
                            El.p [ El.txt "No data dependencies" ]
                          else
                            El.ul
                              (List.map
                                 (fun (r : Obuilder_spec.Rom.t) ->
                                   match r.kind with
                                   | `Build (hash, _dir) ->
                                       El.li
                                         [
                                           El.a
                                             ~at:
                                               [
                                                 At.href
                                                   (Fmt.str "/data/%s" hash);
                                               ]
                                             [ El.txt (String.sub hash 0 12) ];
                                         ])
                                 i.rom)
                        in
                        [
                          base; cmd; El.h3 [ El.txt "Data Dependencies" ]; roms;
                        ]);
                  El.h3 [ El.txt "Logs" ];
                  El.p
                    [
                      El.txt
                        {|Raw logs from the build of this particular pipeline step. The data here is very raw, but can help explain how the data was produced.|};
                    ];
                  pure_button (Fmt.str "/logs/%s" id) "Raw Logs";
                ];
            ];
        ];
      (match geojsons with [] -> El.void | lst -> map lst);
    ]
  |> template title
