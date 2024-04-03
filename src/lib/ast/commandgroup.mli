(** A named basic-block in PL terms. *)

type t [@@deriving sexp]

val v : string -> Leaf.t list -> t
(** Creates a command group made up of a series of leaf nodes and given a name. *)

val pp : t Fmt.t
(** A pretty printer for command groups. *)

val name : t -> string
val children : t -> Leaf.t list
