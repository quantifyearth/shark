type t [@@deriving sexp]

val pp : t Fmt.t
val v : (string * string list) list -> (string * Fpath.t) list -> t
val empty : t
val of_string : string -> (t, [ `Msg of string ]) result
val variables : t -> (string * string list) list

(* These are specific to shark rather than general frontmatter *)
val inputs : t -> Fpath.t list
val input_map : t -> (string * Fpath.t) list
val default_container_path : t -> Fpath.t
