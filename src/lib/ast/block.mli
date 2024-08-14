module Raw : sig
  type t [@@deriving sexp]
  (** A shark block *)

  val build_or_run :
    ?hash:string -> alias:string -> body:string -> [ `Run | `Build ] -> t
  (** Construct a custom block. *)

  val publish : ?output:[ `Directory of string ] -> string -> t
  (** A publish block with a body. Default output is [`Directory "./_shark"] *)

  val import : ?hash:string -> ?alias:string -> string -> t
  (** A shark import statement with a body *)

  val pp : t Fmt.t
  (** A pretty printer for blocks. *)

  val with_hash : t -> string -> t
  (** [with_hash block] is [block] with a new hash. Publish blocks remain unchanged. *)

  val alias : t -> string
  (** A block's alias *)

  val command_list : t -> string list
  (** [command_list block] parses the [block]'s body for commands. *)

  val import_spec : t -> Obuilder_spec.t
  (** For a shark-import block generate the spec to execute to enact the import. *)

  val hash : t -> string option
  (** If a block has been run it will hash a build hash *)

  val kind : t -> [ `Run | `Build | `Publish | `Import ]
  (** The kind of block *)

  val body : t -> string
  (** The body of the block *)

  val output : t -> [ `Directory of string ]
  (** The output of a publish block *)

  val imports : t -> (Uri.t * Fpath.t) list
  (** The imports from an import block i.e. a list of [URL, Path] pairs. *)

  val digest : t -> string
end

type t [@@deriving sexp]

val v : string -> Raw.t -> Leaf.t list -> t
(* Constructor for a new block *)

val kind : t -> [ `Run | `Build | `Publish | `Import ]
(** The kind of block *)

val raw : t -> Raw.t
val hash : t -> string option
val hashes : t -> string list
val update_hash : t -> string -> unit
val commands : t -> Leaf.t list
val context : t -> string
val io : t -> Datafile.t list * Datafile.t list
val digest : t -> string
val pp : t Fmt.t
