open Sexplib.Conv

module CommandResult = struct
  type t = { build_hash : string; output : string option; command : string }
  [@@deriving sexp]

  let v ?output ~build_hash command = { build_hash; output; command }
  let build_hash r = r.build_hash
  let output r = r.output
  let command r = r.command
  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
end
