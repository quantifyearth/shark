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

  val result : t -> CommandResult.t
  val build_hash : t -> string
  val success : t -> bool
  val workdir : t -> string
  val env : t -> (string * string) list
  val pp : t Fmt.t
end
