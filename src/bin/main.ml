open Lwt.Infix

let ( / ) = Filename.concat

module Sandbox = Obuilder.Native_sandbox
module Fetcher = Obuilder.Docker_extract
module Store_spec = Obuilder.Store_spec

let await = Lwt_eio.Promise.await_lwt

let store_of_string = function
  | `Rsync path -> `Rsync (path, Obuilder.Rsync_store.Copy)
  | (`Zfs _ | `Btrfs _ | `Xfs _ | `Docker _) as v -> v

let config_path =
  match Sys.getenv_opt "SHARK_CONFIG" with
  | Some config -> config
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Filename.concat home ".shark"
      | None -> failwith "No SHARK_CONFIG or HOME environment variables")

let store_or_default v =
  match Option.map store_of_string v with
  | Some store -> Obuilder.Store_spec.to_store store
  | None ->
      let config = In_channel.with_open_bin config_path In_channel.input_all in
      let config = Shark.Config.t_of_sexp (Sexplib.Sexp.of_string config) in
      Obuilder.Store_spec.to_store config.store

let run_eventloop ~clock main =
  Lwt_eio.with_event_loop ~debug:true ~clock @@ fun _ ->
  Lwt_eio.Promise.await_lwt (main ())

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output ->
      output_string stdout msg;
      flush stdout

let create_builder (_, spec) conf =
  let (Store_spec.Store ((module Store), store)) = await spec in
  let module Builder = Obuilder.Builder (Store) (Sandbox) (Fetcher) in
  let sandbox =
    await @@ Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  in
  let builder = Builder.v ~store ~sandbox in
  Shark.Md.Builder ((module Builder), builder)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let build () store spec conf src_dir secrets =
  run_eventloop @@ fun () ->
  let store = store_or_default store in
  let (Builder ((module Builder), builder)) = create_builder store conf in
  Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ fun () ->
  let spec =
    try Obuilder.Spec.t_of_sexp (Sexplib.Sexp.load_sexp spec)
    with Failure msg ->
      print_endline msg;
      exit 1
  in
  let secrets =
    List.map (fun (id, path) -> (id, read_whole_file path)) secrets
  in
  let context = Obuilder.Context.v ~log ~src_dir ~secrets () in
  Builder.build builder context spec >>= function
  | Ok x ->
      Fmt.pr "Got: %S@." (x :> string);
      Lwt.return_unit
  | Error `Cancelled ->
      Fmt.epr "Cancelled at user's request@.";
      exit 1
  | Error (`Msg m) ->
      Fmt.epr "Build step failed: %s@." m;
      exit 1

let run () store conf id =
  run_eventloop @@ fun () ->
  let store = store_or_default store in
  let (Builder ((module Builder), builder)) = create_builder store conf in
  Fun.protect ~finally:(fun () -> await @@ Builder.finish builder) @@ fun () ->
  let _, v = Builder.shell builder id in
  v >>= fun v ->
  match v with
  | Ok _ -> Lwt.return_unit
  | Error `Cancelled ->
      Fmt.epr "Cancelled at user's request@.";
      exit 1
  | Error (`Msg m) ->
      Fmt.epr "Build step failed: %s@." m;
      exit 1

let log_warning exn = Eio.traceln "%s" (Printexc.to_string exn)

let edit ~proc ~net ~fs () file port =
  run_eventloop @@ fun () ->
  let port = match port with None -> 8080 | Some port -> port in
  let handler conn request body =
    let routes =
      Shark.Serve.edit_routes ~proc Eio.Path.(fs / file) conn request body
    in
    let router = Routes.one_of routes in
    match Routes.match' router ~target:(Http.Request.resource request) with
    | FullMatch a -> a
    | MatchWithTrailingSlash a -> a
    | NoMatch -> Shark.Serve.respond_not_found
  in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  Eio.traceln "Running edit server at http://%a:%i/editor" Eio.Net.Ipaddr.pp
    Eio.Net.Ipaddr.V4.any port;
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr
  and server = Cohttp_eio.Server.make ~callback:handler () in
  Cohttp_eio.Server.run socket server ~on_error:log_warning

let md ~proc ~net ~fs () no_run store conf file port =
  run_eventloop @@ fun () ->
  let ((_, store) as s) = store_or_default store in
  let (Builder ((module Builder), builder) as obuilder) =
    create_builder s conf
  in
  Fun.protect ~finally:(fun () -> await @@ Builder.finish builder) @@ fun () ->
  let doc =
    In_channel.with_open_bin file @@ fun ic ->
    Cmarkit.Doc.of_string (In_channel.input_all ic)
  in
  let f ~alias_hash_map code_block block =
    if no_run then code_block
    else
      let cb, blk =
        Lwt_eio.Promise.await_lwt
        @@ Shark.Md.process_block ~alias_hash_map:!alias_hash_map obuilder
             (code_block, block)
      in
      alias_hash_map :=
        (Shark.Block.alias blk, Option.get (Shark.Block.hash blk))
        :: !alias_hash_map;
      cb
  in
  let document = Shark.Md.map_blocks doc ~f in
  Eio.Switch.run @@ fun sw ->
  let run_server () =
    match port with
    | None -> Fmt.pr "%s" (Cmarkit_commonmark.of_doc document)
    | Some port ->
        let output_path = Eio.Path.(fs / Filename.temp_file "shark-md" "run") in
        Eio.Path.save ~create:(`If_missing 0o644) output_path
          (Cmarkit_commonmark.of_doc document);
        let handler = Shark.Serve.router ~proc ~fs ~store output_path in
        let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
        Eio.traceln "Running server on %a" Eio.Net.Sockaddr.pp addr;
        let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr
        and server = Cohttp_eio.Server.make ~callback:handler () in
        Cohttp_eio.Server.run socket server ~on_error:log_warning
  in
  Lwt.return (run_server ())

let template ~clock ~fs () file directory =
  run_eventloop ~clock @@ fun () ->
  let file_path = Eio.Path.(fs / file) in
  let directory = Eio.Path.(fs / directory) in
  Shark.Template.template ~file_path ~directory;
  Lwt.return_unit

let config () =
  let config = Shark.Config.{ store = `Zfs "obuilder-zfs" } in
  Fmt.pr "%a" Sexplib.Sexp.pp_hum (Shark.Config.sexp_of_t config)

let dot ~fs () file =
  run_eventloop @@ fun () ->
  let file_path = Eio.Path.(fs / file) in
  let template_markdown = Eio.Path.load file_path in
  let s = Shark.Dotrenderer.render ~template_markdown in
  Format.pp_print_string Format.std_formatter s;
  Lwt.return_unit

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Obuilder.log_src level;
  Logs.set_reporter (Logs_fmt.reporter ());
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
  Arg.required
  @@ Arg.pos 0 Arg.(some dir) None
  @@ Arg.info ~doc:"Directory containing the source files." ~docv:"DIR" []

let store =
  Arg.value
  @@ Arg.opt Arg.(some Store_spec.store_t) None
  @@ Arg.info ~doc:"Store for shark, defaults to configuration file."
       ~docv:"STORE" [ "store" ]

let no_run =
  Arg.value @@ Arg.flag
  @@ Arg.info ~doc:"Don't run any code blocks in the markdown file" [ "no-run" ]

let port =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Optional port number to serve the markdown file over."
       ~docv:"PORT" [ "port" ]

let id =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The $(i,ID) of a build within the store." ~docv:"ID" []

let secrets =
  Arg.value
  @@ Arg.(opt_all (pair ~sep:':' string file)) []
  @@ Arg.info ~doc:"Provide a secret under the form $(b,id:file)."
       ~docv:"SECRET" [ "secret" ]

let build ~clock =
  let doc = "Build a spec file." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info
    Term.(
      const (build ~clock)
      $ setup_log $ store $ spec_file $ Obuilder.Native_sandbox.cmdliner
      $ src_dir $ secrets)

let run ~clock =
  let doc = "Run a shell inside a container" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(
      const (run ~clock)
      $ setup_log $ store $ Obuilder.Native_sandbox.cmdliner $ id)

let md ~proc ~net ~fs ~clock =
  let doc = "Execute a markdown file" in
  let info = Cmd.info "md" ~doc in
  Cmd.v info
    Term.(
      const (md ~proc ~net ~fs ~clock)
      $ setup_log $ no_run $ store $ Obuilder.Native_sandbox.cmdliner
      $ markdown_file $ port)

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
  [
    build ~clock;
    run ~clock;
    md ~proc ~clock ~net ~fs;
    editor ~proc ~clock ~net ~fs;
    config;
    template ~clock (Eio.Stdenv.fs env);
    dot ~clock ~fs;
  ]

let () =
  Eio_main.run @@ fun env ->
  let doc = "a command-line interface for Shark" in
  let info = Cmd.info ~doc "shark" in
  exit (Cmd.eval @@ Cmd.group info (cmds env))
