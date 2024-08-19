(** {1 AST}

The AST is the logical representation of the workflow described in a
sharkdown file, including the structure of groups (aka basic blocks
in PL, but block is an overloaded term in this context). *)

module Leaf = Leaf
module Command = Command
module Metadata = Metadata
module Datafile = Datafile

module Ast : sig
  module Block = Block

  type block_id = int [@@deriving sexp]

  type t [@@deriving sexp]
  (** An AST  *)

  val v :
    nodes:(block_id * Block.t) list ->
    edges:(block_id * block_id) list ->
    Metadata.t ->
    t
  (** Construct a new AST from edges and nodes *)

  val pp : t Fmt.t
  (** A pretty printer for ASTs *)

  val pp_dot : t Fmt.t
  (** The dot graph of the AST. *)

  val find_id_of_block : t -> Block.Raw.t -> block_id option
  val block_by_id : t -> block_id -> Block.t option
  val find_block_from_raw_block : t -> Block.Raw.t -> Block.t option
  val find_dependencies : t -> block_id -> Block.t list
  val default_container_path : t -> Fpath.t

  val to_list : t -> Block.t list
  (** Convert the AST to a list of command blocks. *)
end
