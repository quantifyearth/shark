
open Sexplib.Conv

type t = {
  kind : kind;
  target : string;
} [@@deriving sexp]

and kind = [ `Build of string * string ] [@@deriving sexp]

let of_build ~hash ~build_dir target =
  {
    kind = `Build (hash, build_dir);
    target
  }