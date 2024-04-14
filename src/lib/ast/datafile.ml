open Astring
open Sexplib.Conv

type path = Fpath.t

let path_of_sexp = function
  | Ppx_sexp_conv_lib.Sexp.Atom s -> Fpath.v s
  | List _ -> Fpath.v ""

let sexp_of_path v = Ppx_sexp_conv_lib.Sexp.Atom (Fpath.to_string v)

type t = { id : int; path : path; subpath : string option; wildcard : bool }
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)

let v ?subpath id path =
  let wildcard =
    match subpath with
    | None -> false
    | Some p -> Char.equal p.[String.length p - 1] '*'
  in
  if wildcard then { id; path; subpath = None; wildcard = true }
  else { id; path; subpath; wildcard = false }

let id d = d.id
let path d = d.path
let subpath d = d.subpath
let is_wildcard d = d.wildcard
let compare a b = Int.compare a.id b.id
let is_dir d = Fpath.is_dir_path d.path

let fullpath d =
  match d.subpath with
  | None -> d.path
  | Some s -> Fpath.append d.path (Fpath.v s)
