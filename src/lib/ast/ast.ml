open Sexplib.Conv

type block_id = int [@@deriving sexp]

type t = {
  nodes : (block_id * Block.t) list;
  edges : (block_id * block_id) list;
  metadata : Metadata.t;
}
[@@deriving sexp]

let v ~nodes ~edges metadata = { nodes; edges; metadata }
let to_list ast = List.map snd ast.nodes
