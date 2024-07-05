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

module ExecutionState = struct
  type t = {
    result : CommandResult.t;
    build_hash : string;
    success : bool;
    workdir : string;
    environment : (string * string) list;
  }
  [@@deriving sexp]

  let v result build_hash success workdir environment =
    { result; build_hash; success; workdir; environment }

  let result e = e.result
  let build_hash e = e.build_hash
  let success e = e.success
  let workdir e = e.workdir
  let env e = e.environment
  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
end
