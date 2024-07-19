(** {1 Sharkdown Utilities}

This modules contains utilities for working with Sharkdown (markdown) files.
*)

val map_blocks :
  Cmarkit.Doc.t ->
  f:
    (build_cache:Build_cache.t ->
    Cmarkit.Block.Code_block.t ->
    Block.t ->
    Cmarkit.Block.Code_block.t * [ `Stop of string | `Continue ]) ->
  Cmarkit.Doc.t * string option
(** [map_blocks doc ~f] maps over the markdown document picking out shark {! Block.t}s.
    These are then past to [f]. [alias_hash_map] is a list of aliases to hashes (i.e. build
    hashes) that can be used to run blocks with the specific hash of a previous build coming
    from another block.

    [f] can also halt further processing by returning [`Stop]. The option returned at the
    end if [`Stop reason] was returned at any point is the [reason]. *)

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

val process_build_block :
  ?src_dir:string ->
  ?hb:Ast.Hyperblock.t ->
  builder ->
  Ast.t ->
  Cmarkit.Block.Code_block.t * Block.t ->
  Cmarkit.Block.Code_block.t * Block.t * [ `Stop of string | `Continue ]

val process_run_block :
  ?environment_override:(string * string) list ->
  fs:_ Eio.Path.t ->
  build_cache:Build_cache.t ->
  pool:unit Eio.Pool.t ->
  Obuilder.Store_spec.store ->
  Ast.t ->
  builder ->
  Cmarkit.Block.Code_block.t * Block.t ->
  Cmarkit.Block.Code_block.t * Block.t * [ `Stop of string | `Continue ]

val process_publish_block :
  Obuilder.Store_spec.store ->
  Ast.t ->
  Cmarkit.Block.Code_block.t * Block.t ->
  Cmarkit.Block.Code_block.t * Block.t

val translate_import_block :
  uid:string -> Block.t -> Cmarkit.Block.Code_block.t * Block.t
(** [translate_import_block uid block] will generate an expanded code block that contains a shark-build spec that 
    carries out the actual import when evaluated. *)
