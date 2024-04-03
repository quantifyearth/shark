open Sexplib.Conv

type t = { name : string; children : Leaf.t list } [@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v name children = { name; children }
let name g = g.name
let children g = g.children
