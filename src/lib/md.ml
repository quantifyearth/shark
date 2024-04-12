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

let process_build_block (Builder ((module Builder), builder)) (code_block, block)
    =
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
  | _ -> failwith "expected build"

let process_run_block ~image_hash_map ~data_image_list
    (Builder ((module Builder), builder)) (_code_block, hyperblock) =
  let block = Ast.Hyperblock.block hyperblock in
  match Block.kind block with
  | `Run ->
      let commands = Ast.Hyperblock.commands hyperblock in
      let commands_stripped =
        List.map Leaf.command commands |> List.map Command.to_string
      in
      let build = List.assoc (Block.alias block) image_hash_map in

      let rom =
        List.map
          (fun input_info ->
            let hash, _ = input_info in
            let mount = "/shark/" ^ hash in
            Obuilder_spec.Rom.of_build ~hash ~build_dir:"/data" mount)
          data_image_list
      in

      let links =
        List.concat_map
          (fun input_info ->
            let hash, paths = input_info in
            let open Fpath in
            let base = Fpath.v "/shark" / hash in
            List.concat_map
              (fun (p : Datafile.t) ->
                let p = Datafile.path p in
                let src =
                  base // Option.get (relativize ~root:(Fpath.v "/data/") p)
                in
                let open Obuilder_spec in
                let target_dir, _ = split_base p in
                [
                  run "mkdir -p %s"
                    (Fpath.to_string (Fpath.rem_empty_seg target_dir));
                  run "ln -s %s %s || true"
                    (Fpath.to_string (Fpath.rem_empty_seg src))
                    (Fpath.to_string (Fpath.rem_empty_seg p));
                ])
              paths)
          data_image_list
      in

      let target_dirs l =
        List.map
          (fun d ->
            let p = Datafile.path d in
            let open Obuilder_spec in
            run "mkdir -p %s" (Fpath.to_string (Fpath.parent p)))
          (Leaf.outputs l)
      in

      let spec build_hash pwd leaf =
        let open Obuilder_spec in
        stage ~from:(`Build build_hash)
          ([ user_unix ~uid:0 ~gid:0; workdir pwd ]
          @ target_dirs leaf @ links
          @ [
              run ~network:[ "host" ] ~rom "%s"
                (Command.to_string (Leaf.command leaf));
            ])
      in
      let process (outputs, build_hash, pwd) leaf =
        Logs.info (fun f ->
            f "Running spec %a" Obuilder_spec.pp (spec build_hash pwd leaf));
        let command = Leaf.command leaf in
        match Command.name command with
        | "cd" ->
            Lwt.return
              ( (build_hash, "") :: outputs,
                build_hash,
                Fpath.to_string (List.nth (Command.file_args command) 0) )
        | _ -> (
            let buf = Buffer.create 128 in
            let log = log `Run buf in
            let context = Obuilder.Context.v ~log ~src_dir:"." () in
            Builder.build builder context (spec build_hash pwd leaf)
            >>= function
            | Ok id -> Lwt.return ((id, Buffer.contents buf) :: outputs, id, pwd)
            | Error `Cancelled -> Lwt.fail_with "Cancelled by user"
            | Error (`Msg m) -> Lwt.fail_with m)
      in

      Lwt_list.fold_left_s process ([], build, "/root") commands
      >>= fun (ids_and_output, _hash, _pwd) ->
      let ids_and_output = List.rev ids_and_output in
      let id = List.hd ids_and_output |> fst in
      let body =
        List.fold_left
          (fun s (command, (_, output)) -> s @ [ command; output ])
          []
          (List.combine commands_stripped ids_and_output)
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in
      let block = Block.with_hash block id in
      let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
      Lwt.return (Cmarkit.Block.Code_block.make ~info_string body, block)
  | _ -> failwith "expected run"

let copy ?chown ~src ~dst () =
  let chown =
    match chown with Some uid_gid -> [ "--chown"; uid_gid ] | None -> []
  in
  let cmd = [ "rsync"; "-aHq" ] @ chown @ [ src; dst ] in
  Obuilder.Os.ensure_dir dst;
  Obuilder.Os.sudo cmd

let process_publish_block ~input_hashes
    (Obuilder.Store_spec.Store ((module Store), store)) (_code_block, block) =
  match Block.kind block with
  | `Publish ->
      let process (hash, files) =
        let copy_file file =
          Store.result store hash >>= function
          | None ->
              Lwt.fail_with
                (Fmt.str "No result found for %s whilst publishing %a" hash
                   Fpath.pp (Datafile.path file))
          | Some f ->
              let path = Datafile.path file |> Fpath.to_string in
              copy
                ~src:(Filename.concat (Filename.concat f "rootfs") path)
                ~dst:"./_shark" ()
        in
        Lwt_list.iter_s copy_file files
      in
      Lwt_list.iter_s process input_hashes >>= fun () ->
      Lwt.return (_code_block, block)
  | _ -> failwith "Expected Publish Block"

let validate_dependancy (Obuilder.Store_spec.Store ((module Store), store)) hash outputs =
  Store.result store hash >>= function
  | None -> 
    Lwt.fail_with (Fmt.str "No result found for %s whilst validating dependancies" hash)
  | Some store_path -> (
    let find_files_in_store file =
      let container_path = Datafile.fullpath file |> Fpath.to_string in
      let absolute_path = (Filename.concat (Filename.concat store_path "rootfs") container_path) in
      match Datafile.is_wildcard file with
      | false -> Lwt_unix.file_exists absolute_path >>= (function
        | false -> failwith "File not found"
        | true -> [(Fpath.v absolute_path)]
        )
      | true -> (
        Lwt_unix.files_of_directory absolute_path |> Lwt_stream.to_list >>= (fun x ->
        Lwt.return (List.filter_map (fun (path : string) ->
          match path with
          | "." | ".." -> None
          | p -> Some (
            Fpath.v p
          )
        ) x))
      )
    in
    Lwt_list.map_s find_files_in_store outputs
  )
