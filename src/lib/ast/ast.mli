(** {1 AST}

The AST is the logical representation of the workflow described in a
sharkdown file, including the structure of groups (aka basic blocks
in PL, but block is an overloaded term in this context). *)

type t
(** An AST instance *)

val order_command_list : Fpath.t list -> (string * Command.t list) list -> t
(** Takes the sharkdown frontmatter and a list of named CommandGroups and builds
    an AST from them.

    TODOs: Don't take in all of the frontmatter just what we need? The CommandGroups
    should probably be a recursive data structure? *)

val to_list : t -> Commandgroup.t list
(** Convert the AST to a list of command blocks. *)
