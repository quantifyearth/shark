type t [@@deriving sexp]
(** The build cache is a runtime data structure for tracking
    the build IDs of image aliases (like Docker tags) *)

val v : unit -> t
(** A fresh build cache store *)

val find : t -> string -> Obuilder.S.id option
(** [find t alias] looks for [alias] in the build cache and returns
    the build ID if it is there *)

val find_exn : t -> string -> Obuilder.S.id
(** Like {! find} except raises [Failure] if not found *)

val with_build :
  t ->
  (t -> (string * Obuilder.S.id * 'a, string * 'a) result) ->
  (string * Obuilder.S.id * 'a, string * 'a) result
(** [with_build t fn] runs [fn] that returns an alias and build ID that
    are added to the build cache store *)
