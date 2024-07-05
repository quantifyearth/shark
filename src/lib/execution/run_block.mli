module CommandResult : sig
  type t [@@deriving sexp]

  val v : ?output:string -> build_hash:string -> string -> t
  val build_hash : t -> string
  val output : t -> string option
  val command : t -> string
  val pp : t Fmt.t
end
