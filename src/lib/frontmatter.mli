type t [@@deriving sexp]

val pp : t Fmt.t
val v : (string * string list) list -> (string * Fpath.t) list -> t
val empty : t
val of_string : string -> (t, [ `Msg of string ]) result
val variables : t -> (string * string list) list
val inputs : t -> Fpath.t list
val input_map : t -> (string * Fpath.t) list
