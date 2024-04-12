open Sexplib.Conv

type id = Obuilder.S.id

let id_of_sexp = function
  | Sexplib.Sexp.Atom s -> s
  | List _ -> invalid_arg "Expected atom for id"

let sexp_of_id id = Sexplib.Sexp.Atom id

type t = { mutable alias_to_build_hash : (string * id) list } [@@deriving sexp]

let v () = { alias_to_build_hash = [] }
let find t alias = List.assoc_opt alias t.alias_to_build_hash

let find_exn t alias =
  match find t alias with
  | Some v -> v
  | None -> failwith ("No build ID found for " ^ alias)

let add t ~alias ~id =
  t.alias_to_build_hash <- (alias, id) :: t.alias_to_build_hash

let with_build t fn =
  let alias, id, res = fn t in
  add t ~alias ~id;
  (alias, id, res)
