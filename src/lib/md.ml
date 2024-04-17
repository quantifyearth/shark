open Astring
open Eio

let ( / ) = Eio.Path.( / )

module CommandResult = struct
  type t = { build_hash : string; output : string; command : string }

  let v ~build_hash ~output ~command = { build_hash; output; command }
  let _build_hash r = r.build_hash
  let output r = r.output
  let command r = r.command
end

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

let process_build_block ?(src_dir = ".") (Builder ((module Builder), builder))
    ast (code_block, block) =
  match Block.kind block with
  | `Build -> (
      let spec =
        Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string (Block.body block))
      in
      let buf = Buffer.create 128 in
      let log = log `Build buf in
      let context = Obuilder.Context.v ~log ~src_dir () in
      match Lwt_eio.run_lwt @@ fun () -> Builder.build builder context spec with
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
          (new_code_block, block_with_hash))
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

let get_paths ~fs (Obuilder.Store_spec.Store ((module Store), store)) hash
    outputs =
  let shark_mount_path = Fpath.add_seg (Fpath.v "/shark") hash in
  match Lwt_eio.run_lwt @@ fun () -> Store.result store hash with
  | None ->
      failwith
        (Fmt.str "No result found for %s whilst validating dependencies" hash)
  | Some store_path ->
      let rootfs = Fpath.add_seg (Fpath.v store_path) "rootfs" in
      let find_files_in_store file =
        let container_path = Datafile.fullpath file in
        let root = Fpath.v "/" in
        let absolute_path =
          Fpath.relativize ~root container_path
          |> Option.get |> Fpath.append rootfs |> Fpath.to_string
        in
        let shark_destination_path =
          let root = Fpath.v "/data/" in
          Fpath.relativize ~root container_path
          |> Option.get
          |> Fpath.append shark_mount_path
        in
        match Datafile.is_wildcard file with
        | false -> (
            match Path.kind ~follow:true (fs / absolute_path) with
            | `Not_found -> (file, [])
            | _ -> (file, [ shark_destination_path ]))
        | true ->
            let files = Path.read_dir (fs / absolute_path) in
            ( file,
              List.filter_map
                (fun (path : string) ->
                  match path with
                  | "." | ".." -> None
                  | p -> Some (Fpath.add_seg shark_destination_path p))
                files )
      in
      List.map find_files_in_store outputs

let process_run_block ~fs ~build_cache ~pool store ast
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

      let spec build_hash pwd environment leaf cmdstr =
        let open Obuilder_spec in
        stage ~from:(`Build build_hash)
          ([ user_unix ~uid:0 ~gid:0; workdir pwd ]
          @ List.map (fun (k, v) -> env k v) environment
          @ target_dirs leaf
          (* @ links *)
          @ [ run ~network:[ "host" ] ~rom "%s" cmdstr ])
      in
      let process pool (_outputs, build_hash, pwd, env) leaf cmdstr :
          CommandResult.t * string * string * (string * string) list =
        Eio.Pool.use pool @@ fun () ->
        Logs.info (fun f ->
            f "Running spec %a" Obuilder_spec.pp
              (spec build_hash pwd env leaf cmdstr));
        let command = Leaf.command leaf in
        match Command.name command with
        | "cd" ->
            ( CommandResult.v ~build_hash ~output:"" ~command:cmdstr,
              build_hash,
              Fpath.to_string (List.nth (Command.file_args command) 0),
              env )
        | "export" ->
            let parts =
              String.concat (List.tl (Command.raw_args command))
              |> String.cuts ~sep:"="
            in
            let key = List.nth parts 0 and value = List.nth parts 1 in
            ( CommandResult.v ~build_hash ~output:"" ~command:cmdstr,
              build_hash,
              pwd,
              (key, value) :: List.remove_assoc key env )
        | _ -> (
            let buf = Buffer.create 128 in
            let log = log `Run buf in
            let context = Obuilder.Context.v ~log ~src_dir:"." () in
            match
              Lwt_eio.run_lwt @@ fun () ->
              Builder.build builder context
                (spec build_hash pwd env leaf cmdstr)
            with
            | Ok id ->
                ( CommandResult.v ~build_hash:id ~output:(Buffer.contents buf)
                    ~command:cmdstr,
                  id,
                  pwd,
                  env )
            | Error `Cancelled -> failwith "Cancelled by user"
            | Error (`Msg _m) ->
                ( CommandResult.v ~build_hash ~output:(Buffer.contents buf)
                    ~command:cmdstr,
                  build_hash,
                  pwd,
                  env ))
      in

      let outer_process acc leaf =
        let inputs = Leaf.inputs leaf in
        let input_and_hashes =
          List.filter_map
            (fun i ->
              match List.assoc_opt (Datafile.id i) input_map with
              | None -> None
              | Some x -> Some (i, x))
            inputs
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
        let paths =
          List.map
            (fun (hash, ref_fd_list) -> get_paths ~fs store hash !ref_fd_list)
            hash_to_input_map
        in
        let l =
          List.fold_left
            (fun a v ->
              let s =
                List.map
                  (fun (arg_path, targets) ->
                    ( Fpath.to_string (Datafile.fullpath arg_path),
                      List.map Fpath.to_string targets ))
                  v
              in
              s @ a)
            [] paths
        in
        let inputs = Leaf.to_string_for_inputs leaf l in
        let l = Fiber.List.map (process pool acc leaf) inputs in
        let results, _hash, _pwd, _env = acc in
        let _, hash, pwd, env = List.hd l in
        (l :: results, hash, pwd, env)
      in

      let ids_and_output_and_cmd, _hash, _pwd, _env =
        List.fold_left outer_process ([], build, "/root", []) commands
      in
      let last = List.hd ids_and_output_and_cmd in
      let _, id, _, _ = List.hd last in

      let body =
        List.fold_left
          (fun s (r, _, _, _) ->
            s @ [ CommandResult.command r; CommandResult.output r ])
          []
          (List.concat (List.rev ids_and_output_and_cmd))
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in

      List.iter
        (fun (_, id, _, _) -> Ast.Hyperblock.update_hash hyperblock id)
        last;
      let block = Block.with_hash block id in
      let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
      (Cmarkit.Block.Code_block.make ~info_string body, block)
  | _ -> failwith "expected run"

let copy ?chown ~src ~dst () =
  Lwt_eio.run_lwt @@ fun () ->
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
      Logs.info (fun f -> f "Inputs for publish");
      List.iter
        (fun (hash, files) ->
          Logs.info (fun f ->
              f "hash: %s has %d files\n" hash (List.length files)))
        inputs;
      let process (hash, files) =
        let copy_file file =
          match Lwt_eio.run_lwt @@ fun () -> Store.result store hash with
          | None ->
              Fmt.str "No result found for %s whilst publishing %a" hash
                Fpath.pp (Datafile.path file)
          | Some f -> (
              let root = Fpath.v "/" in
              let path =
                Datafile.path file |> Fpath.relativize ~root |> Option.get
                |> Fpath.to_string
              in
              let src = Filename.concat (Filename.concat f "rootfs") path in
              try
                copy ~src ~dst:"./_shark" ();
                src
              with Failure msg -> Fmt.str "Failed to copy %s: %s" src msg)
        in
        List.map copy_file files
      in
      let outputs =
        List.map process inputs |> List.concat
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in
      let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
      (Cmarkit.Block.Code_block.make ~info_string outputs, block)
  | _ -> failwith "Expected Publish Block"
