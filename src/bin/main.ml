open Lwt.Infix

let ( / ) = Filename.concat

module Sandbox = Obuilder.Sandbox
module Fetcher = Obuilder.Docker
module Store_spec = Obuilder.Store_spec

let await = Lwt_eio.Promise.await_lwt

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

let run_eventloop main =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~debug:true ~clock @@ fun _ ->
  Lwt_eio.Promise.await_lwt (main ())

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output ->
      output_string stdout msg;
      flush stdout

let create_builder spec conf =
  let (Store_spec.Store ((module Store), store)) = await spec in
  let module Builder = Obuilder.Builder (Store) (Sandbox) (Fetcher) in
  let sandbox =
    await @@ Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  in
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let build () store spec conf src_dir secrets =
  run_eventloop @@ fun () ->
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

let option_get = function Some v -> v | None -> failwith "Dumbass!"

let md () store conf file =
  run_eventloop @@ fun () ->
  let (Builder ((module Builder), builder)) = create_builder store conf in
  Fun.protect ~finally:(fun () -> await @@ Builder.finish builder) @@ fun () ->
  let doc =
    In_channel.with_open_bin file @@ fun ic ->
    Cmarkit.Doc.of_string (In_channel.input_all ic)
  in
  let fn alias_hash_map code_block block =
    let cb, blk =
      Lwt_eio.Promise.await_lwt
      @@ Shark.Md.process_block !alias_hash_map store conf (code_block, block)
    in
    alias_hash_map := (blk.alias, option_get blk.hash) :: !alias_hash_map;
    cb
  in

  let document = Shark.Md.map_blocks doc fn in
  Fmt.pr "%s" (Cmarkit_commonmark.of_doc document);
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
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of markdown file." ~docv:"MD" [ "md" ]

let src_dir =
  Arg.required
  @@ Arg.pos 0 Arg.(some dir) None
  @@ Arg.info ~doc:"Directory containing the source files." ~docv:"DIR" []

let store = Store_spec.cmdliner

let id =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The $(i,ID) of a build within the store." ~docv:"ID" []

let secrets =
  Arg.value
  @@ Arg.(opt_all (pair ~sep:':' string file)) []
  @@ Arg.info ~doc:"Provide a secret under the form $(b,id:file)."
       ~docv:"SECRET" [ "secret" ]

let build =
  let doc = "Build a spec file." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info
    Term.(
      const build $ setup_log $ store $ spec_file $ Obuilder.Sandbox.cmdliner
      $ src_dir $ secrets)

let run =
  let doc = "Run a shell inside a container" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(const run $ setup_log $ store $ Obuilder.Sandbox.cmdliner $ id)

let md =
  let doc = "Execute a markdown file" in
  let info = Cmd.info "md" ~doc in
  Cmd.v info
    Term.(
      const md $ setup_log $ store $ Obuilder.Sandbox.cmdliner $ markdown_file)

let cmds = [ build; run; md ]

let () =
  let doc = "a command-line interface for Shark" in
  let info = Cmd.info ~doc "shark" in
  exit (Cmd.eval @@ Cmd.group info cmds)
