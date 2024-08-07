(** A Leaf is an atomic execution unit the in the pipeline graph. *)

type style = Command | Map
type t [@@deriving sexp]

val v : int -> Command.t -> style -> Datafile.t list -> Datafile.t list -> t
(** Creats a new leaf node, taking an integer identifier, the command to execute
      and a list of inputs and a list of outputs. *)

val pp : t Fmt.t
(** A pretty printer for leaves. *)

val id : t -> int
val command : t -> Command.t
val command_style : t -> style
val inputs : t -> Datafile.t list
val outputs : t -> Datafile.t list
val to_string_for_inputs : t -> (string * string list) list -> string list
