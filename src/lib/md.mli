(** {1 Sharkdown Utilities}

This modules contains utilities for working with Sharkdown (markdown) files.
*)

val map_blocks :
  Cmarkit.Doc.t ->
  f:
    (image_hash_map:(string * string) list ref ->
    data_hash_map:(Digest.t * string) list ref ->
    Cmarkit.Block.Code_block.t ->
    Block.t ->
    Cmarkit.Block.Code_block.t) ->
  Cmarkit.Doc.t
(** [map_blocks doc ~f] maps over the markdown document picking out shark {! Block.t}s.
    These are then past to [f]. [alias_hash_map] is a list of aliases to hashes (i.e. build
    hashes) that can be used to run blocks with the specific hash of a previous build coming
    from another block. *)

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

val process_build_block :
  builder ->
  Cmarkit.Block.Code_block.t * Block.t ->
  (Cmarkit.Block.Code_block.t * Block.t) Lwt.t

val process_run_block :
  image_hash_map:(string * string) list ->
  data_image_list:(string * Datafile.t list) list ->
  builder ->
  Cmarkit.Block.Code_block.t * Block.t ->
  (Cmarkit.Block.Code_block.t * Block.t) Lwt.t
