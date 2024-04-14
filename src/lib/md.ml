open Lwt.Infix

let map_blocks (doc : Cmarkit.Doc.t) ~f =
  let build_cache = Build_cache.v () in
  let block _mapper = function
    | Cmarkit.Block.Code_block (node, meta) -> (
        match Block.of_code_block node with
        | Some block ->
            let new_block = f ~build_cache node block in
            `Map (Some (Cmarkit.Block.Code_block (new_block, meta)))
        | None -> `Default)
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

let process_build_block (Builder ((module Builder), builder)) ast
    (code_block, block) =
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
          let block_with_hash = Block.with_hash block id in
          (* Update hyperblock hash *)
          let hb = Ast.find_hyperblock_from_block ast block |> Option.get in
          Ast.Hyperblock.update_hash hb id;
          let new_code_block =
            let info_string = Block.to_info_string block in
            Cmarkit.Block.Code_block.make
              ~info_string:(info_string, Cmarkit.Meta.none)
              (Cmarkit.Block.Code_block.code code_block)
          in
          Lwt.return (new_code_block, block_with_hash))
  | _ -> failwith "expected build"

let input_hashes ast block =
  let block_id = Option.get (Ast.find_id_of_block ast block) in
  let block_dependencies = Ast.find_dependencies ast block_id in

  (* The input Datafile has the wildcard flag, which won't be set on the
     output flag, so we need to swap them over *)
  let input_map =
    Ast.Hyperblock.io (Option.get (Ast.block_by_id ast block_id))
    |> fst
    |> List.map (fun df -> (Datafile.id df, df))
  in

  let map_to_inputs hb =
    let hashes = Ast.Hyperblock.hashes hb in
    let inputs =
      Ast.Hyperblock.io hb |> snd |> List.map Datafile.id
      |> List.filter_map (fun o -> List.assoc_opt o input_map)
    in
    List.map (fun h -> (h, inputs)) hashes
  in
  List.concat_map map_to_inputs block_dependencies

let get_paths (Obuilder.Store_spec.Store ((module Store), store)) hash outputs =
  let shark_mount_path = Fpath.add_seg (Fpath.v "/shark") hash in
  Store.result store hash >>= function
  | None ->
      Lwt.fail_with
        (Fmt.str "No result found for %s whilst validating dependancies" hash)
  | Some store_path ->
      let rootfs = Fpath.add_seg (Fpath.v store_path) "rootfs" in
      let find_files_in_store file =
        let container_path = Datafile.fullpath file in
        let root = Fpath.v "/" in
        let absolute_path =
          Fpath.relativize ~root container_path
          |> Option.get |> Fpath.append rootfs
        in
        let shark_destination_path =
          let root = Fpath.v "/data/" in
          Fpath.relativize ~root container_path
          |> Option.get
          |> Fpath.append shark_mount_path
        in
        match Datafile.is_wildcard file with
        | false -> (
            Lwt_unix.file_exists (Fpath.to_string absolute_path) >>= function
            | false -> Lwt.return (file, [])
            | true -> Lwt.return (file, [ shark_destination_path ]))
        | true ->
            Lwt_unix.files_of_directory (Fpath.to_string absolute_path)
            |> Lwt_stream.to_list
            >>= fun x ->
            Lwt.return
              ( file,
                List.filter_map
                  (fun (path : string) ->
                    match path with
                    | "." | ".." -> None
                    | p -> Some (Fpath.add_seg shark_destination_path p))
                  x )
      in
      Lwt_list.map_s find_files_in_store outputs

let process_run_block ~build_cache store ast
    (Builder ((module Builder), builder)) (_code_block, block) =
  let hyperblock = Ast.find_hyperblock_from_block ast block |> Option.get in
  match Block.kind block with
  | `Run ->
      let commands = Ast.Hyperblock.commands hyperblock in
      let inputs = input_hashes ast block in
      let build = Build_cache.find_exn build_cache (Block.alias block) in

      let rom =
        List.map
          (fun (hash, _) ->
            let mount = "/shark/" ^ hash in
            Obuilder_spec.Rom.of_build ~hash ~build_dir:"/data" mount)
          inputs
      in

      let input_map =
        List.map
          (fun (hash, dfs) -> List.map (fun df -> (Datafile.id df, hash)) dfs)
          inputs
        |> List.concat
      in

      let target_dirs l =
        List.map
          (fun d ->
            let p = Datafile.fullpath d in
            let open Obuilder_spec in
            let target =
              match Datafile.is_dir d with false -> Fpath.parent p | true -> p
            in
            run "mkdir -p %s" (Fpath.to_string target))
          (Leaf.outputs l)
      in

      let spec build_hash pwd leaf cmdstr =
        let open Obuilder_spec in
        stage ~from:(`Build build_hash)
          ([ user_unix ~uid:0 ~gid:0; workdir pwd ]
          @ target_dirs leaf
          (* @ links *)
          @ [ run ~network:[ "host" ] ~rom "%s" cmdstr ])
      in
      let process (_outputs, build_hash, pwd) leaf cmdstr :
          ((string * string * string) * string * string) Lwt.t =
        Logs.info (fun f ->
            f "Running spec %a" Obuilder_spec.pp
              (spec build_hash pwd leaf cmdstr));
        let command = Leaf.command leaf in
        match Command.name command with
        | "cd" ->
            Lwt.return
              ( (build_hash, "", cmdstr),
                build_hash,
                Fpath.to_string (List.nth (Command.file_args command) 0) )
        | _ -> (
            let buf = Buffer.create 128 in
            let log = log `Run buf in
            let context = Obuilder.Context.v ~log ~src_dir:"." () in
            Builder.build builder context (spec build_hash pwd leaf cmdstr)
            >>= function
            | Ok id -> Lwt.return ((id, Buffer.contents buf, cmdstr), id, pwd)
            | Error `Cancelled -> Lwt.fail_with "Cancelled by user"
            | Error (`Msg m) ->
                Printf.printf "output: %s\n" (Buffer.contents buf);
                Lwt.fail_with m)
      in

      let outer_process acc leaf =
        let inputs = Leaf.inputs leaf in
        let input_and_hashes =
          List.map (fun i -> (i, List.assoc (Datafile.id i) input_map)) inputs
        in
        let hash_to_input_map =
          List.fold_left
            (fun a (df, hash) ->
              match List.assoc_opt hash a with
              | None -> (hash, ref [ df ]) :: a
              | Some l ->
                  l := df :: !l;
                  a)
            [] input_and_hashes
        in
        List.map
          (fun (hash, ref_fd_list) -> get_paths store hash !ref_fd_list)
          hash_to_input_map
        |> Lwt.all
        >>= Lwt_list.fold_left_s
              (fun a v ->
                let s =
                  List.map
                    (fun (arg_path, targets) ->
                      ( Fpath.to_string (Datafile.fullpath arg_path),
                        List.map Fpath.to_string targets ))
                    v
                in
                Lwt.return (s @ a))
              []
        >>= fun l ->
        Lwt.return (Leaf.to_string_for_inputs leaf l)
        >>= Lwt_list.map_p (fun c -> process acc leaf c)
        >>= fun l ->
        let results, _hash, _pwd = acc in
        let _, hash, pwd = List.hd l in
        Lwt.return (l :: results, hash, pwd)
        (*
        >>= Lwt_list.fold_left_s
              (fun a v ->
                let outputs, _build_hash, _pwd, commands = a
                and no, nh, np, command = v in
                Lwt.return (no :: outputs, nh, np, command :: commands))
              (acc_outputs, "", "", acc_commands) *)
      in

      Lwt_list.fold_left_s outer_process ([], build, "/root") commands
      >>= fun (ids_and_output_and_cmd, _hash, _pwd) ->
      let ids_and_output_and_cmd = List.rev ids_and_output_and_cmd in
      let last = List.hd ids_and_output_and_cmd in
      let _, id, _ = List.hd last in

      let body =
        List.fold_left
          (fun s (_, output, command) -> s @ [ command; output ])
          []
          (List.concat ids_and_output_and_cmd)
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in

      List.iter
        (fun (_, id, _) -> Ast.Hyperblock.update_hash hyperblock id)
        last;
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

let process_publish_block (Obuilder.Store_spec.Store ((module Store), store))
    ast (_code_block, block) =
  match Block.kind block with
  | `Publish ->
      let inputs = input_hashes ast block in
      Printf.printf "inputs\n";
      List.iter
        (fun (hash, files) ->
          Printf.printf "\thash: %s has %d files\n" hash (List.length files))
        inputs;
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
      Lwt_list.iter_s process inputs >>= fun () ->
      Lwt.return (_code_block, block)
  | _ -> failwith "Expected Publish Block"
