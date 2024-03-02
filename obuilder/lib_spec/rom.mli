type t = {
  kind : kind;
  target : string;
} [@@deriving sexp]

and kind = [ `Build of string * string ] [@@deriving sexp]

val of_build : hash:string -> build_dir:string -> string -> t
(** Construct a read-only mount from a previous build and a specific directory
    in that build that will be mounted on the target. *)