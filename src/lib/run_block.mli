module CommandResult : sig
  type t [@@deriving sexp]

  val v : ?output:string -> build_hash:string -> string -> t
  val build_hash : t -> string
  val output : t -> string option
  val command : t -> string
  val pp : t Fmt.t
end

module ExecutionState : sig
  type t [@@deriving sexp]

  val v :
    result:CommandResult.t ->
    build_hash:string ->
    success:bool ->
    workdir:string ->
    environment:(string * string) list ->
    t

  val init :
    build_hash:string ->
    workdir:string ->
    environment:(string * string) list ->
    t

  val result : t -> CommandResult.t
  val build_hash : t -> string
  val success : t -> bool
  val workdir : t -> string
  val env : t -> (string * string) list
  val pp : t Fmt.t
end

val process_single_command_execution :
  previous_state:ExecutionState.t ->
  environment_override:(string * string) list ->
  command_leaf:Leaf.t ->
  file_subs_map:(string * string list) list ->
  run_f:
    (ExecutionState.t ->
    Leaf.t ->
    string ->
    Buffer.t ->
    (string, string option * string) result) ->
  string ->
  ExecutionState.t
(** [process_single_command_execution previous_state environment_override command_leaf file_map task_runner command_string] 
  evaluates the [command_string] based on the state of the previous execution [previous_state]. Certain commands
  will just update the state machine (e.g., cd and export), but commands that are side effect driven will
  invoke [task_runner] to do the actual execution - most likely with Obuilder, but the point here is to keep 
  that stuff external to allow for easier testing.
  
  Currently this has a complicated interface as it was before part of the main md run look - the aim is over
  time to try clean this up. *)
