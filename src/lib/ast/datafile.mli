(** A named file/directory that acts as an input and/or output of a process. *)

type t [@@deriving sexp]

val v : ?subpath:string -> int -> Fpath.t -> t
(** Creates a new datafile with an integer ID and a file path. *)

val pp : t Fmt.t
(** A pretty printer for datafiles. *)

val id : t -> int
val path : t -> Fpath.t
val subpath : t -> string option
val fullpath : t -> Fpath.t
val is_wildcard : t -> bool
val is_dir : t -> bool
val compare : t -> t -> int