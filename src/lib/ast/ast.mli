(** {1 AST}

The AST is the logical representation of the workflow described in a
sharkdown file, including the structure of groups (aka basic blocks
in PL, but block is an overloaded term in this context). *)

type t
(** An AST instance *)

val of_sharkdown : template_markdown:string -> t 

val to_list : t -> Commandgroup.t list
(** Convert the AST to a list of command blocks. *)
