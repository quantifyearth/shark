open Sexplib.Std

type id = string [@@deriving sexp_of]

type tag = [
  | `Heading    (** Introduces a new build step *)
  | `Note       (** Informational output from OBuilder *)
  | `Output     (** Raw output from the build command *)
]

type logger = tag -> string -> unit

module type STORE = sig
  type t

  val build :
    t -> ?base:id ->
    id:id ->
    (string -> (unit, 'e) Lwt_result.t) ->
    (unit, 'e) Lwt_result.t
  (** [build t ~id fn] runs [fn tmpdir] to add a new item to the store under
      key [id]. On success, [tmpdir] is saved as [id], which can be used
      as the [base] for further builds, until it is expired from the cache.
      On failure, nothing is recorded and calling [build] again will make
      another attempt at building it.
      @param base Initialise [tmpdir] as a clone of [base]. *)

  val result : t -> id -> string option
  (** [result t id] is the path of the build result for [id], if present. *)

  val state_dir : t -> string
  (** [state_dir] is the path of a directory which can be used to store mutable
      state related to this store (e.g. an sqlite3 database). *)
end

module type SANDBOX = sig
  type t

  val run :
    cancelled:unit Lwt.t ->
    ?stdin:Os.unix_fd ->
    log:Build_log.t ->
    t ->
    Config.t ->
    string ->
    (unit, [`Cancelled | `Msg of string]) Lwt_result.t
  (** [run ~cancelled t config dir] runs the operation [config] in a sandbox with root
      filesystem [rootfs].
      @param cancelled Resolving this kills the process (and returns [`Cancelled]).
      @param stdin Passed to child as its standard input.
      @param log Used for child's stdout and stderr.
  *)
end
