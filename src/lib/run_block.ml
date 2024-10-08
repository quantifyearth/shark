open Astring
open Sexplib.Conv
open Import
open Shark_ast

module CommandResult = struct
  type t = { build_hash : string; output : string option; command : string }
  [@@deriving sexp]

  let v ?output ~build_hash command = { build_hash; output; command }
  let build_hash r = r.build_hash
  let output r = r.output
  let command r = r.command
  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
end

module ExecutionState = struct
  type t = {
    result : CommandResult.t;
    build_hash : string;
    success : bool;
    workdir : string;
    environment : (string * string) list;
  }
  [@@deriving sexp]

  let v ~result ~build_hash ~success ~workdir ~environment =
    { result; build_hash; success; workdir; environment }

  let init ~build_hash ~workdir ~environment =
    {
      result = CommandResult.v ~build_hash "";
      build_hash;
      success = true;
      workdir;
      environment;
    }

  let change_dir e dst =
    let result =
      CommandResult.v ~build_hash:e.build_hash (Fmt.str "cd %s" dst)
    in
    { e with result; workdir = dst }

  let update_env e key value =
    let result =
      CommandResult.v ~build_hash:e.build_hash
        (Fmt.str "export %s=%s" key value)
    in
    let updated_env = (key, value) :: List.remove_assoc key e.environment in
    { e with result; environment = updated_env }

  let command_fail e result = { e with result; success = false }
  let result e = e.result
  let build_hash e = e.build_hash
  let success e = e.success
  let workdir e = e.workdir
  let env e = e.environment
  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
end

let process_single_command_execution ~previous_state ~environment_override
    ~command_leaf ~file_joins ~file_subs_map ~run_f expanded_command_string =
  let command = Leaf.command command_leaf in
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
            String.cut ~sep:" " expanded_command_string
            |> Option.get ~err:"Failed to get path in cd"
            |> snd
        | _ -> (
            let path = Fpath.to_string (List.nth args 0) in
            match List.assoc_opt path file_subs_map with
            | None -> path
            | Some [] -> path
            | Some pl -> List.nth pl 0)
      in
      ExecutionState.change_dir previous_state inspected_path
  | "export" ->
      (* `export` is treated like ENV in Docker, only supporting a single key=value for now. *)
      let key, default_value =
        String.concat (List.tl (Command.raw_args command))
        |> String.cut ~sep:"="
        |> function
        | Some (k, v) -> (k, v)
        | None -> Fmt.failwith "Malformed export command: %a" Command.pp command
      in
      let value =
        match List.assoc_opt key environment_override with
        | None -> default_value
        | Some v -> v
      in
      ExecutionState.update_env previous_state key value
  | _ -> (
      (* Otherwise we run a command using obuilder or such *)
      let buf = Buffer.create 128 in
      let res =
        run_f previous_state command_leaf file_joins expanded_command_string buf
      in
      match res with
      | Ok id ->
          ExecutionState.v
            ~result:
              (CommandResult.v ~build_hash:id ~output:(Buffer.contents buf)
                 expanded_command_string)
            ~build_hash:id ~success:true
            ~workdir:(ExecutionState.workdir previous_state)
            ~environment:(ExecutionState.env previous_state)
      | Error (id_opt, msg) -> (
          match id_opt with
          | Some id ->
              let cmd_result =
                CommandResult.v ~build_hash:id
                  ~output:(msg ^ "\n" ^ Buffer.contents buf)
                  expanded_command_string
              in
              ExecutionState.command_fail previous_state cmd_result
          | None ->
              let old_id = ExecutionState.build_hash previous_state in
              let cmd_result =
                CommandResult.v ~build_hash:old_id
                  ~output:(msg ^ "\n" ^ Buffer.contents buf)
                  expanded_command_string
              in
              ExecutionState.command_fail previous_state cmd_result))
