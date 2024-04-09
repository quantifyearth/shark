(** {1 Shark Blocks}
    
A shark block is a code block in a markdown file. These blocks 
are distinguished by either [shark-build] or [shark-run] as the
code blocks "language".

After [shark-build] and separated by a [:] there is the build's alias
which [shark-run] commands can reference in the same way to tell
{e shark} to use that build environment to execute the command.

[shark-publish] blocks are different. They allow you to export data
from shark along with its build description. At the moment only
exporting to the local filesystem is supported. *)

type t
(** A shark block *)

val build_or_run :
  ?hash:string -> alias:string -> body:string -> [ `Run | `Build ] -> t
(** Construct a custom block. *)

val publish : ?output:[ `Directory of string ] -> string -> t
(** A publish block with a body. Default output is [`Directory "./_shark"] *)

val pp : t Fmt.t
(** A pretty printer for blocks. *)

val with_hash : t -> string -> t
(** [with_hash block] is [block] with a new hash. Publish blocks remain unchanged. *)

val of_info_string :
  ?default:(info:string -> body:string -> t option) ->
  body:string ->
  string ->
  t option
(** [of_info_string ~body info] creates a shark block from a code block's
    body and the info string (the bit after [```] usually). You can use [default]
    to override the value that is returned if the [info_string] is not a
    Shark block. *)

val to_info_string : t -> string
(** Convert the block back to the info string *)

val command_list : t -> string list
(** [command_list block] parses the [block]'s body for commands. *)

val alias : t -> string
(** A block's alias *)

val hash : t -> string option
(** If a block has been run it will hash a build hash *)

val kind : t -> [ `Run | `Build | `Publish ]
(** The kind of block *)

val body : t -> string
(** The body of the block *)

val output : t -> [ `Directory of string ]
val digest : t -> string
