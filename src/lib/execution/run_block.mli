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
  Obuilder_spec.Rom.t list ->
  (string * string) list ->
  Leaf.t ->
  (string * string list) list ->
  (Obuilder_spec.t -> Buffer.t -> (string, string option * string) result) ->
  string ->
  ExecutionState.t
