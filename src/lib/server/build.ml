let ( / ) = Filename.concat

let find_paths pred paths =
  let rec loop acc = function
    | `Dir (_, more) -> List.fold_left loop acc more
    | `File (path, _) -> if pred path then path :: acc else acc
    | _ -> acc
  in
  loop [] paths

(* Rendering Builds *)
let render (Obuilder.Store_spec.Store ((module Store), store)) id =
  let result, inputs =
    Lwt_eio.run_lwt (fun () ->
        let open Lwt.Syntax in
        let* result = Store.result store id in
        let+ inputs = Store.get_meta store id ":obuilder-run-input" in
        ( result,
          match inputs with
          | Some inputs ->
              Some
                (Obuilder.S.run_input_of_sexp (Sexplib.Sexp.of_string inputs))
          | None -> None ))
  in
  match result with
  | None ->
      Cohttp_eio.Server.respond_string ~status:`OK ~body:("No result for " ^ id)
        ()
  | Some path ->
      let manifest, geojsons, jsons, images, tabular =
        let src_dir = Fpath.(v path / "rootfs") |> Fpath.to_string in
        match Obuilder.Manifest.generate ~exclude:[] ~src_dir "data" with
        | Error (`Msg m) -> (m, [], [], [], [])
        | Ok src_manifest ->
            let geojsons =
              find_paths
                (fun p -> Filename.extension p = ".geojson")
                src_manifest
            in
            let jsons =
              find_paths (fun p -> Filename.extension p = ".json") src_manifest
            in
            let geojsons =
              List.map Uri.pct_encode geojsons
              |> List.map (fun v ->
                     "." / id
                     / (Uri.with_query (Uri.of_string "serve")
                          [ ("file", [ v ]) ]
                       |> Uri.to_string))
            in
            let jsons =
              List.map
                (fun v ->
                  In_channel.with_open_bin (Filename.concat src_dir v)
                  @@ In_channel.input_all)
                jsons
            in
            let images =
              find_paths
                (fun p ->
                  Filename.extension p = ".png"
                  || Filename.extension p = ".jpeg")
                src_manifest
            in
            let images =
              List.map Uri.pct_encode images
              |> List.map (fun v ->
                     "." / id
                     / (Uri.with_query (Uri.of_string "serve")
                          [ ("file", [ v ]) ]
                       |> Uri.to_string))
            in
            let table =
              let csvs =
                find_paths (fun p -> Filename.extension p = ".csv") src_manifest
              in
              List.fold_left
                (fun acc data ->
                  ( In_channel.with_open_bin (Filename.concat src_dir data)
                  @@ fun ic -> Csv.load_in ic )
                  :: acc)
                [] csvs
            in
            ( Obuilder.Manifest.sexp_of_t src_manifest
              |> Sexplib.Sexp.to_string_hum,
              geojsons,
              jsons,
              images,
              table )
      in
      let page =
        Pages.build ~geojsons ~jsons ~images ~tabular ~manifest ~title:"Build"
          ~id ?inputs ()
      in
      let body =
        Cohttp_eio.Body.of_string (Htmlit.El.to_string ~doctype:true page)
      in
      let headers =
        (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
        Cohttp.Header.init_with "X-Accel-Buffering" "no"
      in
      Cohttp_eio.Server.respond ~status:`OK ~headers ~body ()
