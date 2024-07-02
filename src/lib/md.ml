open Astring
open Eio
open Import

let ( / ) = Eio.Path.( / )

module CommandResult = struct
  type t = { build_hash : string; output : string option; command : string }

  let v ?output ~build_hash command = { build_hash; output; command }
  let _build_hash r = r.build_hash
  let output r = r.output
  let command r = r.command
end

let map_blocks (doc : Cmarkit.Doc.t) ~f =
  let build_cache = Build_cache.v () in
  let stop_processing = ref None in
  let block _mapper v =
    if Option.is_some !stop_processing then `Default
    else
      match v with
      | Cmarkit.Block.Code_block (node, meta) -> (
          match Block.of_code_block node with
          | Some block ->
              let new_block, continue = f ~build_cache node block in
              (match continue with
              | `Continue -> ()
              | `Stop reason -> stop_processing := Some reason);
              `Map (Some (Cmarkit.Block.Code_block (new_block, meta)))
          | None -> `Default)
      | _ -> `Default
  in
  let mapper = Cmarkit.Mapper.make ~block () in
  let doc = Cmarkit.Mapper.map_doc mapper doc in
  (doc, !stop_processing)

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

let process_build_block ?(src_dir = ".") ?hb
    (Builder ((module Builder), builder)) ast (code_block, block) =
  match Block.kind block with
  | `Build -> (
      let spec =
        Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string (Block.body block))
      in
      let buf = Buffer.create 128 in
      let log = log `Build buf in
      let context = Obuilder.Context.v ~log ~src_dir () in
      match Lwt_eio.run_lwt @@ fun () -> Builder.build builder context spec with
      | Error `Cancelled -> (code_block, block, `Stop "Cancelled by user")
      | Error (`Msg m) -> (code_block, block, `Stop m)
      | Error (`Failed (id, msg)) ->
          let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
          let body = Cmarkit.Block_line.list_of_string (Buffer.contents buf) in
          let cb = Cmarkit.Block.Code_block.make ~info_string body in
          (cb, block, `Stop (Printf.sprintf "%s: %s" id msg))
      | Ok id ->
          let block_with_hash = Block.with_hash block id in
          (* Update hyperblock hash *)
          let hb =
            match hb with
            | Some hb -> hb
            | None ->
                Ast.find_hyperblock_from_block ast block
                |> Option.get ~err:"No hyperblock for build block"
          in
          Ast.Hyperblock.update_hash hb id;
          let new_code_block =
            let info_string = Block.to_info_string block in
            Cmarkit.Block.Code_block.make
              ~info_string:(info_string, Cmarkit.Meta.none)
              (Cmarkit.Block.Code_block.code code_block)
          in
          (new_code_block, block_with_hash, `Continue))
  | _ -> failwith "expected build"

let input_hashes ast block =
  let block_id =
    Option.get ~err:"No block ID for input hashes"
      (Ast.find_id_of_block ast block)
  in
  let block_dependencies = Ast.find_dependencies ast block_id in

  (* The input Datafile has the wildcard flag, which won't be set on the
     output flag, so we need to swap them over *)
  let input_map =
    Ast.Hyperblock.io
      (Option.get ~err:"No block ID for input map"
         (Ast.block_by_id ast block_id))
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
          |> Option.get ~err:"Relativizing paths"
          |> Fpath.append rootfs |> Fpath.to_string
        in
        let shark_destination_path =
          let root = Fpath.v "/data/" in
          Fpath.relativize ~root container_path
          |> Option.get ~err:"Relativizing paths"
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
                  | p -> (
                      match Filename.extension p with
                      | ".shark" -> None
                      | _ -> Some (Fpath.add_seg shark_destination_path p)))
                files )
      in
      List.map find_files_in_store outputs

type processed_output = {
  cmd_result : CommandResult.t;
  success : bool;
  build_hash : Obuilder.S.id;
  workdir : string;
  env : (string * string) list;
}

let process_run_block ?(env_override = []) ~fs ~build_cache ~pool store ast
    (Builder ((module Builder), builder)) (_code_block, block) =
  let hyperblock =
    Ast.find_hyperblock_from_block ast block
    |> Option.get ~err:"No hyperblock for run block"
  in
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

      let spec ~build_hash ~workdir environment leaf cmdstr =
        Obuilder_spec.stage ~from:(`Build build_hash)
          ([
             Obuilder_spec.user_unix ~uid:0 ~gid:0;
             Obuilder_spec.workdir workdir;
           ]
          @ List.map (fun (k, v) -> Obuilder_spec.env k v) environment
          @ target_dirs leaf
          @ [ Obuilder_spec.run ~network:[ "host" ] ~rom "%s" cmdstr ])
      in

      let process pool (_outputs, build_hash, workdir, env) leaf file_subs_map
          cmdstr =
        Eio.Pool.use pool @@ fun () ->
        let command = Leaf.command leaf in
        Logs.info (fun f -> f "Processing command: %a" Command.pp command);
        match Command.name command with
        | "cd" ->
            (* If a command block is a call to `cd` we treat this similarly to Docker's
               WORKDIR command which changes the working directory of the context *)

            (* If the dir is in the inputs we should substitute it, otherwise we assume it's a new dir in this
               current image. *)
            let args = Command.file_args command in
            let inspected_path =
              match args with
              | [] ->
                  (* no /data path in this, so just pull the path directly as the AST only works with /data paths *)
                  String.cut ~sep:" " (Command.to_string command)
                  |> Option.get ~err:"Failed to get path in cd"
                  |> snd
              | _ -> (
                  let path = Fpath.to_string (List.nth args 0) in
                  match List.assoc_opt path file_subs_map with
                  | None -> path
                  | Some pl -> (
                      match pl with [] -> path | _ -> List.nth pl 0))
            in

            let cmd_result = CommandResult.v ~build_hash cmdstr in
            {
              cmd_result;
              build_hash;
              success = true;
              workdir = inspected_path;
              env;
            }
        | "export" ->
            (* `export` is treated like ENV in Docker, only supporting a single key=value for now. *)
            let key, default_value =
              String.concat (List.tl (Command.raw_args command))
              |> String.cut ~sep:"="
              |> function
              | Some (k, v) -> (k, v)
              | None ->
                  Fmt.failwith "Malformed export command: %a" Command.pp command
            in
            let value =
              match List.assoc_opt key env_override with
              | None -> default_value
              | Some v -> v
            in
            let cmd_result =
              CommandResult.v ~build_hash (Fmt.str "export %s=%s" key value)
            in
            {
              cmd_result;
              build_hash;
              success = true;
              workdir;
              env = (key, value) :: List.remove_assoc key env;
            }
        | _ -> (
            (* Otherwise we run a command using obuilder *)
            let buf = Buffer.create 128 in
            let log = log `Run buf in
            let context = Obuilder.Context.v ~log ~src_dir:"." () in
            let spec = spec ~build_hash ~workdir env leaf cmdstr in
            Logs.info (fun f -> f "Running spec: %a" Obuilder_spec.pp spec);
            match
              Lwt_eio.run_lwt @@ fun () -> Builder.build builder context spec
            with
            | Ok id ->
                {
                  cmd_result =
                    CommandResult.v ~build_hash:id ~output:(Buffer.contents buf)
                      cmdstr;
                  build_hash = id;
                  success = true;
                  workdir;
                  env;
                }
            | Error `Cancelled -> failwith "Cancelled by user"
            | Error (`Msg m) -> failwith m
            | Error (`Failed (id, msg)) ->
                let cmd_result =
                  CommandResult.v ~build_hash:id
                    ~output:(msg ^ "\n" ^ Buffer.contents buf)
                    cmdstr
                in
                { cmd_result; success = false; build_hash; workdir; env })
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
        (* Sanity check whether we found the matching inputs *)
        List.iter
          (fun (i, s) ->
            match s with
            | [] ->
                Fmt.failwith "Failed to find source files for input %s of %s" i
                  (Command.name (Leaf.command leaf))
            | _ -> ())
          l;
        let inputs = Leaf.to_string_for_inputs leaf l in
        let processed_blocks =
          Fiber.List.map (process pool acc leaf l) inputs
        in
        let results, _hash, _pwd, _env = acc in
        let { build_hash; workdir; env; _ } =
          match processed_blocks with
          | hd :: _ -> hd
          | [] ->
              Fmt.failwith "There were no processed blocks for %s"
                (Command.name (Leaf.command leaf))
        in
        (processed_blocks :: results, build_hash, workdir, env)
      in

      let ids_and_output_and_cmd, _hash, _pwd, _env =
        List.fold_left outer_process ([], build, "/root", []) commands
      in
      let last = List.hd ids_and_output_and_cmd in
      let { build_hash = id; _ } = List.hd last in

      let body =
        List.fold_left
          (fun s { cmd_result = r; _ } ->
            s
            @ [
                CommandResult.command r;
                (match CommandResult.output r with Some o -> o | None -> "");
              ])
          []
          (List.concat (List.rev ids_and_output_and_cmd))
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in

      List.iter
        (fun { build_hash = id; _ } -> Ast.Hyperblock.update_hash hyperblock id)
        last;
      let block = Block.with_hash block id in
      let info_string = (Block.to_info_string block, Cmarkit.Meta.none) in
      (* TODO: We should be able to continue procressing other blocks if only one fails
         here, but I would like to restructure the code to support this better and have
         ideas for that. For now, a single failure here will stop the procressing. *)
      let stop = List.find_opt (fun { success; _ } -> not success) last in
      let action =
        match stop with
        | None -> `Continue
        | Some r -> (
            match r.cmd_result.output with
            | Some o -> `Stop o
            | None -> `Stop "No output")
      in

      (Cmarkit.Block.Code_block.make ~info_string body, block, action)
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
                Datafile.path file |> Fpath.relativize ~root
                |> Option.get ~err:"Relativizing paths"
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

let translate_import_block ~uid block =
  match Block.kind block with
  | `Import ->
      let spec, src_dir_opt = Block.import_spec block in
      Logs.info (fun f -> f "import spec: %a" Obuilder_spec.pp spec);
      let body = Sexplib.Sexp.to_string_hum (Obuilder_spec.sexp_of_t spec) in
      let alias = Fmt.str "import-statement-%s" uid in
      let block = Block.build_or_run ~alias ~body `Build in
      let code_block =
        Cmarkit.Block.Code_block.make
          ~info_string:(Fmt.str "shark-build:%s" alias, Cmarkit.Meta.none)
          (Cmarkit.Block_line.list_of_string body)
      in
      ((code_block, block), src_dir_opt)
  | _ -> failwith "Expected Import Block"
