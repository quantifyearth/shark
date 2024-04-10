type t [@@deriving sexp]

val v : name:string -> args:string list -> file_args:Fpath.t list -> t

val pp : t Fmt.t
(** A pretty printer for blocks. *)

val of_string : string -> t option
val to_string : t -> string
val name : t -> string
val file_args : t -> Fpath.t list
