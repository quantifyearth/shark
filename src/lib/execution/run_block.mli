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
    CommandResult.t -> string -> bool -> string -> (string * string) list -> t

  val init : string -> string -> (string * string) list -> t
  val result : t -> CommandResult.t
  val build_hash : t -> string
  val success : t -> bool
  val workdir : t -> string
  val env : t -> (string * string) list
  val pp : t Fmt.t
end

val process_single_command_execution :
  ExecutionState.t ->
  (string * string) list ->
  Leaf.t ->
  (string * string list) list ->
  (ExecutionState.t ->
  Leaf.t ->
  string ->
  Buffer.t ->
  (string, string option * string) result) ->
  string ->
  ExecutionState.t
(** [process_single_command_execution previous_state environment_override leaf file_map task_runner command_string] 
  evaluates the [command_string] based on the state of the previous exection [previous_state]. Certain commands
  will just update the state machine (e.g., cd and export), but commands that are side effect driven will
  invoke [task_runner] to do the actual exection - most likely with Obuilder, but the point here is to keep 
  that stuff external to allow for easier testing.
  
  Currently this has a complicated interface as it was before part of the main md run look - the aim is over
  time to try clean this up. *)
