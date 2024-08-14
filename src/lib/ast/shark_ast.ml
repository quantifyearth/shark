open Sexplib.Conv
module Leaf = Leaf
module Command = Command
module Metadata = Metadata
module Datafile = Datafile

module Ast = struct
  module Block = Block
  include Ast

  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
  let pp_dot = Dot.render

  let find_id_of_block ast ib =
    let d = Block.Raw.digest ib in
    let rec loop l =
      match l with
      | [] -> None
      | hd :: tl ->
          let id, hb = hd in
          if Block.digest hb = d then Some id else loop tl
    in
    loop ast.nodes

  let block_by_id ast id = List.assoc_opt id ast.nodes

  let find_dependencies ast id =
    List.filter_map
      (fun (edge : block_id * block_id) : block_id option ->
        let from, too = edge in
        if too = id then Some from else None)
      ast.edges
    |> List.sort_uniq (fun a b -> a - b)
       (* remove duplicates if we take more than one output from a block *)
    |> List.map (fun id -> List.assoc id ast.nodes)

  let find_block_from_raw_block ast block =
    let id = find_id_of_block ast block in
    Option.bind id (block_by_id ast)

  let default_container_path ast = Metadata.default_container_path ast.metadata
end
