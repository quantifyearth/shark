open Lwt.Infix

let map_blocks (doc : Cmarkit.Doc.t) ~f =
  let image_hash_map = ref [] in
  let data_hash_map = ref [] in

  let block _mapper = function
    | Cmarkit.Block.Code_block (node, meta) -> (
        match Cmarkit.Block.Code_block.info_string node with
        | None -> `Default
        | Some (s, _) -> (
            let body = Cmarkit.Block.Code_block.code node in
            let body =
              List.map Cmarkit.Block_line.to_string body |> String.concat "\n"
            in
            match Block.of_info_string ~body s with
            | Some block ->
                let new_block = f ~image_hash_map ~data_hash_map node block in
                `Map (Some (Cmarkit.Block.Code_block (new_block, meta)))
            | None -> `Default))
    | _ -> `Default
  in
  let mapper = Cmarkit.Mapper.make ~block () in
  Cmarkit.Mapper.map_doc mapper doc

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Sandbox = Obuilder.Native_sandbox
module Fetcher = Obuilder.Docker_extract
module Store_spec = Obuilder.Store_spec

let log kind buffer tag msg =
  match tag with
  | `Heading -> (
      match kind with `Build -> Buffer.add_string buffer msg | `Run -> ())
  | `Note -> (
      match kind with `Build -> Buffer.add_string buffer msg | `Run -> ())
  | `Output -> Buffer.add_string buffer msg


let process_build_block (Builder ((module Builder), builder)) (code_block, block) =
  match Block.kind block with
  | `Build -> (
      let spec =
        Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string (Block.body block))
      in
      let buf = Buffer.create 128 in
      let log = log `Build buf in
      let context = Obuilder.Context.v ~log ~src_dir:"." () in
      Builder.build builder context spec >>= function
      | Error `Cancelled -> failwith "Cancelled by user"
      | Error (`Msg m) -> failwith m
      | Ok id ->
          let block = Block.with_hash block id in
          let new_code_block =
            let info_string = Block.to_info_string block in
            Cmarkit.Block.Code_block.make
              ~info_string:(info_string, Cmarkit.Meta.none)
              (Cmarkit.Block.Code_block.code code_block)
          in
          Lwt.return (new_code_block, block))
  | `Run -> failwith "expected build"

let process_run_block ~image_hash_map ~data_image_list (Builder ((module Builder), builder))
    (_code_block, block) =
  match Block.kind block with
  | `Build -> failwith "expected run"
  | `Run ->
      let commands =
        String.split_on_char '\n' (Block.body block)
        |> List.filter (String.starts_with ~prefix:"$ ")
      in
      let commands_stripped =
        List.map (fun s -> String.sub s 1 (String.length s - 1)) commands
      in
      let build = List.assoc (Block.alias block) image_hash_map in
      let rom = List.map (fun input_info ->
        let hash, _ = input_info in 
        let mount = "/shark/" ^ hash in
        Obuilder_spec.Rom.of_build ~hash:hash ~build_dir:"/data" mount
      ) data_image_list in
      let links = List.concat_map (fun input_info ->
        let hash, paths = input_info in
        let open Fpath in 
        let base = (Fpath.v "/shark") / hash in
        List.concat_map (fun (p : Datafile.t) ->
          let p = Datafile.path p in
          let src = base // (Option.get (relativize ~root:(Fpath.v "/data/") p)) in
          let open Obuilder_spec in
          let target_dir, _ = split_base p in
          [
            run "mkdir -p %s" (Fpath.to_string target_dir) ;
            run "ln -s %s %s || true" (Fpath.to_string src) (Fpath.to_string p);
          ]
        ) paths
      ) data_image_list in
      let spec build_hash command =
        let open Obuilder_spec in
        stage ~from:(`Build build_hash) ([ 
        user_unix ~uid:0 ~gid:0;
        workdir "/root"] @ links @ [run ~network:["host"] ~rom "%s" command ])
      in
      let process (outputs, build_hash) command =
        Logs.info (fun f ->
            f "Running spec %a" Obuilder_spec.pp (spec build_hash command));
        let buf = Buffer.create 128 in
        let log = log `Run buf in
        let context = Obuilder.Context.v ~log ~src_dir:"." () in
        Builder.build builder context (spec build_hash command) >>= function
        | Ok id -> Lwt.return ((id, Buffer.contents buf) :: outputs, id)
        | Error `Cancelled -> Lwt.fail_with "Cancelled by user"
        | Error (`Msg m) -> Lwt.fail_with m
      in
      Lwt_list.fold_left_s process ([], build) commands_stripped
      >>= fun (ids_and_output, _hash) ->
      let ids_and_output = List.rev ids_and_output in
      let id = List.hd ids_and_output |> fst in
      let body =
        List.fold_left
          (fun s (command, (_, output)) -> s @ [ command; output ])
          []
          (List.combine commands ids_and_output)
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in
      let block = Block.with_hash block id in
      let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
      Lwt.return (Cmarkit.Block.Code_block.make ~info_string body, block)
