open Shark_ast
(** {1 Markdown Blocks}
  
   We map between markdown code blocks and Sharks {! Shark_ast.Block}.
*)

type t = Ast.Block.Raw.t

val of_info_string :
  ?default:(info:string -> body:string -> Ast.Block.Raw.t option) ->
  body:string ->
  string ->
  t option
(** [of_info_string ~body info] creates a shark block from a code block's
    body and the info string (the bit after [```] usually). You can use [default]
    to override the value that is returned if the [info_string] is not a
    Shark block. *)

val of_code_block :
  ?default:(info:string -> body:string -> t option) ->
  Cmarkit.Block.Code_block.t ->
  t option
(** Like {! of_info_string} but pulls out the data from code block for you *)

val to_info_string : t -> string
(** Convert the block back to the info string *)
