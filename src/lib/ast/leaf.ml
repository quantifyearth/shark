open Sexplib.Conv

type style = Command | Map [@@deriving sexp]

type t = {
  id : int;
  command : Command.t;
  style : style;
  inputs : Datafile.t list;
  outputs : Datafile.t list;
}
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v id command style inputs outputs = { id; command; style; inputs; outputs }
let command o = o.command
let inputs o = o.inputs
let outputs o = o.outputs
let command_style o = o.style
let id o = o.id
