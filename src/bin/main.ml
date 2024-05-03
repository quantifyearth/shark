let ( / ) = Filename.concat

module Sandbox = Obuilder.Native_sandbox
module Store_spec = Obuilder.Store_spec

let _config_path =
  match Sys.getenv_opt "SHARK_CONFIG" with
  | Some config -> config
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Filename.concat home ".shark"
      | None -> failwith "No SHARK_CONFIG or HOME environment variables")

(* TODO: Default store config is disabled for now *)
let store_or_default = function store -> store
(* | None ->
    let config = In_channel.with_open_bin config_path In_channel.input_all in
    let config = Shark.Config.t_of_sexp (Sexplib.Sexp.of_string config) in
    Obuilder.Store_spec.to_store config.store *)

let run_eventloop ~clock main =
  Lwt_eio.with_event_loop ~clock @@ fun _ -> main ()

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output ->
      output_string stdout msg;
      flush stdout

let fetcher_of_string s =
  match String.lowercase_ascii s with
  | "docker" -> `Docker
  | "container-image" -> `Container_image
  | s -> Fmt.failwith "Unknown fetching backend: %s" s

let create_builder ~fs ~net ~domain_mgr fetcher (_, spec) conf =
  let (Store_spec.Store ((module Store), store)) =
    Lwt_eio.run_lwt @@ fun () -> spec
  in
  let (module Fetcher) =
    match fetcher_of_string fetcher with
    | `Docker -> (module Obuilder.Docker.Extract : Obuilder.S.FETCHER)
    | `Container_image ->
        Obuilder.Container_image_extract.make_fetcher ~progress:true ~fs ~net
          domain_mgr
  in
  let module Builder = Obuilder.Builder (Store) (Sandbox) (Fetcher) in
  let sandbox =
    Lwt_eio.run_lwt @@ fun () ->
    Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  in
  let builder = Builder.v ~store ~sandbox in
  Shark.Md.Builder ((module Builder), builder)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let build ~fs ~net ~domain_mgr () store spec conf src_dir secrets fetcher =
  run_eventloop @@ fun () ->
  let store = store_or_default store in
  let (Builder ((module Builder), builder)) =
    create_builder ~fs ~net ~domain_mgr fetcher store conf
  in
  Fun.protect ~finally:(fun () ->
      Lwt_eio.run_lwt @@ fun () -> Builder.finish builder)
  @@ fun () ->
  let spec = Obuilder.Spec.t_of_sexp (Sexplib.Sexp.load_sexp spec) in
  let secrets =
    List.map (fun (id, path) -> (id, read_whole_file path)) secrets
  in
  let context = Obuilder.Context.v ~log ~src_dir ~secrets () in
  match Lwt_eio.run_lwt @@ fun () -> Builder.build builder context spec with
  | Ok x ->
      Fmt.pr "Got: %S@." (x :> string);
      Ok ()
  | Error `Cancelled -> Error "Cancelled at user's request"
  | Error (`Msg m) -> Error (Fmt.str "Build step failed: %s" m)
  | Error (`Failed (id, m)) -> Error (Fmt.str "Build %s failed: %s" id m)

let run ~fs ~net ~domain_mgr () store conf id fetcher =
  run_eventloop @@ fun () ->
  let store = store_or_default store in
  let (Builder ((module Builder), builder)) =
    create_builder ~fs ~net ~domain_mgr fetcher store conf
  in
  Fun.protect ~finally:(fun () ->
      Lwt_eio.run_lwt @@ fun () -> Builder.finish builder)
  @@ fun () ->
  let v = Lwt_eio.run_lwt @@ fun () -> Builder.shell builder id |> snd in
  match v with
  | Ok _ as v -> v
  | Error `Cancelled -> Error "Cancelled at user's request@."
  | Error (`Msg m) -> Error (Fmt.str "Build step failed: %s" m)

let log_warning exn = Eio.traceln "%s" (Printexc.to_string exn)

let edit ~proc ~net ~fs () file port =
  run_eventloop @@ fun () ->
  let port = match port with None -> 8080 | Some port -> port in
  let handler conn request body =
    let routes =
      Shark_server.edit_routes ~proc Eio.Path.(fs / file) conn request body
    in
    let router = Routes.one_of routes in
    match Routes.match' router ~target:(Http.Request.resource request) with
    | FullMatch a -> a
    | MatchWithTrailingSlash a -> a
    | NoMatch -> Shark_server.respond_not_found
  in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  Eio.traceln "Running edit server at http://%a:%i/editor" Eio.Net.Ipaddr.pp
    Eio.Net.Ipaddr.V4.any port;
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr
  and server = Cohttp_eio.Server.make ~callback:handler () in
  Cohttp_eio.Server.run socket server ~on_error:log_warning

let md ~fs ~net ~domain_mgr ~proc () no_run store conf file port fetcher jobs
    src_dir env_override =
  run_eventloop @@ fun () ->
  let ((_, store) as s) = store_or_default store in
  let (Builder ((module Builder), _builder) as obuilder) =
    create_builder ~fs ~net ~domain_mgr fetcher s conf
  in
  Fun.protect ~finally:(fun () -> ()) @@ fun () ->
  let doc =
    In_channel.with_open_bin file @@ fun ic ->
    Cmarkit.Doc.of_string (In_channel.input_all ic)
  in

  let file_path = Eio.Path.(fs / file) in
  let template_markdown = Eio.Path.load file_path in
  let ast = Shark.Ast.of_sharkdown ~template_markdown in

  let pool = Eio.Pool.create jobs (fun () -> ()) in
  let store = Lwt_eio.run_lwt @@ fun () -> store in
  let f ~build_cache code_block block =
    if no_run then (code_block, `Continue)
    else
      match Shark.Block.kind block with
      | `Publish ->
          let cb, _blk =
            Shark.Md.process_publish_block store ast (code_block, block)
          in
          (cb, `Continue)
      | `Build ->
          let _alias, _id, cb =
            Shark.Build_cache.with_build build_cache @@ fun _build_cache ->
            let cb, blk =
              Shark.Md.process_build_block ~src_dir obuilder ast
                (code_block, block)
            in
            (Shark.Block.alias blk, Option.get (Shark.Block.hash blk), cb)
          in
          (cb, `Continue)
      | `Run ->
          let cb, _result_block, stop =
            Shark.Md.process_run_block ~env_override ~fs ~build_cache ~pool
              store ast obuilder (code_block, block)
          in
          (cb, stop)
  in

  let document, stopped = Shark.Md.map_blocks doc ~f in
  let doc_string = Cmarkit_commonmark.of_doc document in
  Eio.Switch.run @@ fun sw ->
  let run () =
    match port with
    | None -> (
        match stopped with
        | Some reason ->
            Fmt.epr "%a\n%s"
              Fmt.(styled (`Fg `Red) string)
              ("BUILD FAILED: " ^ reason)
              doc_string;
            Error "Build failed"
        | None ->
            Fmt.pr "%s" doc_string;
            Ok ())
    | Some port ->
        let output_path = Eio.Path.(fs / Filename.temp_file "shark-md" "run") in
        Eio.Path.save ~create:(`If_missing 0o644) output_path
          (Cmarkit_commonmark.of_doc document);
        let handler = Shark_server.router ~proc ~fs ~store output_path in
        let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
        Eio.traceln "Running server on %a" Eio.Net.Sockaddr.pp addr;
        let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr
        and server = Cohttp_eio.Server.make ~callback:handler () in
        Cohttp_eio.Server.run socket server ~on_error:log_warning
  in
  run ()

let template ~clock ~fs () file directory =
  run_eventloop ~clock @@ fun () ->
  let file_path = Eio.Path.(fs / file) in
  let directory = Eio.Path.(fs / directory) in
  Shark.Template.template ~file_path ~directory;
  Ok ()

let config () =
  let config = Shark.Config.{ store = `Zfs (None, "obuilder-zfs", false) } in
  Fmt.pr "%a" Sexplib.Sexp.pp_hum (Shark.Config.sexp_of_t config);
  Ok ()

let dot ~fs () file =
  run_eventloop @@ fun () ->
  let file_path = Eio.Path.(fs / file) in
  let template_markdown = Eio.Path.load file_path in
  let s = Shark.Dotrenderer.render ~template_markdown in
  Format.pp_print_string Format.std_formatter s;
  Ok ()

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Obuilder.log_src level;
  Logs.set_reporter (Logging.reporter (Mtime_clock.counter ()) Fmt.stdout);
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let spec_file =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of build spec file." ~docv:"FILE" [ "f" ]

let markdown_file =
  Arg.required
  @@ Arg.pos 0 Arg.(some file) None
  @@ Arg.info ~doc:"Path of markdown file." ~docv:"MARKDOWN_FILE" []

let output_directory =
  Arg.required
  @@ Arg.opt Arg.(some dir) None
  @@ Arg.info ~doc:"Path of output directory for templating." ~docv:"OUTDIR"
       [ "output-dir" ]

let src_dir =
  Arg.value
  @@ Arg.opt Arg.(dir) "."
  @@ Arg.info ~doc:"Directory containing the source files." ~docv:"SRC_DIR"
       [ "src_dir" ]

let store = Store_spec.cmdliner

let no_run =
  Arg.value @@ Arg.flag
  @@ Arg.info ~doc:"Don't run any code blocks in the markdown file" [ "no-run" ]

let port =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Optional port number to serve the markdown file over."
       ~docv:"PORT" [ "port" ]

let jobs =
  Arg.value
  @@ Arg.opt Arg.(int) 4
  @@ Arg.info ~doc:"Maximum number of blocks to evaluate concurrently"
       ~docv:"JOBS" [ "j" ]

let fetcher =
  Arg.required
  @@ Arg.opt Arg.(some string) (Some "docker")
  @@ Arg.info ~doc:"Which image fetching backend to use." ~docv:"FETCHER"
       [ "fetcher" ]

let id =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The $(i,ID) of a build within the store." ~docv:"ID" []

let secrets =
  Arg.value
  @@ Arg.(opt_all (pair ~sep:':' string file)) []
  @@ Arg.info ~doc:"Provide a secret under the form $(b,id:file)."
       ~docv:"SECRET" [ "secret" ]

let env_override =
  Arg.value
  @@ Arg.(opt_all (pair ~sep:'=' string string)) []
  @@ Arg.info
       ~doc:
         "Provide alternative values for environment variables in the style \
          KEY=VALUE."
       ~docv:"ENVIRONMENT" [ "e" ]

let build ~fs ~net ~domain_mgr ~clock =
  let doc = "Build a spec file." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info
    Term.(
      const (build ~fs ~net ~domain_mgr ~clock)
      $ setup_log $ store $ spec_file $ Obuilder.Native_sandbox.cmdliner
      $ src_dir $ secrets $ fetcher)

let run ~fs ~net ~domain_mgr ~clock =
  let doc = "Run a shell inside a container" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(
      const (run ~fs ~net ~domain_mgr ~clock)
      $ setup_log $ store $ Obuilder.Native_sandbox.cmdliner $ id $ fetcher)

let md ~fs ~net ~domain_mgr ~proc ~clock =
  let doc = "Execute a markdown file" in
  let info = Cmd.info "md" ~doc in
  Cmd.v info
    Term.(
      const (md ~fs ~net ~domain_mgr ~proc ~clock)
      $ setup_log $ no_run $ store $ Obuilder.Native_sandbox.cmdliner
      $ markdown_file $ port $ fetcher $ jobs $ src_dir $ env_override)

let editor ~proc ~net ~fs ~clock =
  let doc = "Run the editor for a markdown file" in
  let info = Cmd.info "editor" ~doc in
  Cmd.v info
    Term.(const (edit ~proc ~net ~fs ~clock) $ setup_log $ markdown_file $ port)

let template ~clock fs =
  let doc = "Template a markdown file by replacing variables" in
  let info = Cmd.info "template" ~doc in
  let cmd = template ~clock ~fs in
  Cmd.v info Term.(const cmd $ setup_log $ markdown_file $ output_directory)

let config =
  let doc = "Parse a config or generate the default" in
  let info = Cmd.info "config" ~doc in
  Cmd.v info Term.(const config $ setup_log)

let dot ~clock ~fs =
  let doc = "Render a markdown file as a graph" in
  let info = Cmd.info "dot" ~doc in
  let cmd = dot ~clock ~fs in
  Cmd.v info Term.(const cmd $ setup_log $ markdown_file)

let cmds env =
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  [
    build ~fs ~net ~domain_mgr ~clock;
    run ~fs ~net ~domain_mgr ~clock;
    md ~fs ~net ~domain_mgr ~proc ~clock;
    editor ~proc ~clock ~net ~fs;
    config;
    template ~clock (Eio.Stdenv.fs env);
    dot ~clock ~fs;
  ]

let () =
  Eio_posix.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let doc = "a command-line interface for Shark" in
  let info = Cmd.info ~doc "shark" in
  exit (Cmd.eval_result @@ Cmd.group info (cmds env))
