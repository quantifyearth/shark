(** {1 AST}

The AST is the logical representation of the workflow described in a
sharkdown file, including the structure of groups (aka basic blocks
in PL, but block is an overloaded term in this context). *)

module Leaf : sig
  (** A Leaf is an atomic exection unit the in the pipeline graph. *)

  type style = Command | Map
  type t

  val v : int -> Command.t -> style -> Datafile.t list -> Datafile.t list -> t
  (** Creats a new leaf node, taking an integer identifier, the command to execute
      and a list of inputs and a list of outputs. *)

  val pp : t Fmt.t
  (** A pretty printer for leaves. *)

  val id : t -> int
  val command : t -> Command.t
  val command_style : t -> style
  val inputs : t -> Datafile.t list
  val outputs : t -> Datafile.t list
end

module CommandGroup : sig
  (** A named basic-block in PL terms. *)

  type t

  val v : string -> Leaf.t list -> t
  (** Creates a command group made up of a series of leaf nodes and given a name. *)

  val pp : t Fmt.t
  (** A pretty printer for command groups. *)

  val name : t -> string
  val children : t -> Leaf.t list
end

type t
(** An AST instance *)

val order_command_list : Frontmatter.t -> (string * Command.t list) list -> t
(** Takes the sharkdown frontmatter and a list of named CommandGroups and builds
    an AST from them.

    TODOs: Don't take in all of the frontmatter just what we need? The CommandGroups
    should probably be a recursive data structure? *)

val to_list : t -> CommandGroup.t list
(** Convert the AST to a list of command blocks. *)
