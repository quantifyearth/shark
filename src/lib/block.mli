(** {1 Shark Blocks}
    
A shark block is a code block in a markdown file. These blocks 
are distinguished by either [shark-build] or [shark-run] as the
code blocks "language".

After [shark-build] and separated by a [:] there is the build's alias
which [shark-run] commands can reference in the same way to tell
{e shark} to use that build environment to execute the command. *)

type t
(** A shark block *)

type kind = [ `Build | `Run ]

val with_hash : t -> string -> t
(** [with_hash block] is [block] with a new hash. *)

val of_info_string : body:string -> string -> t option
(** [of_info_string ~body info] creates a shark block from a code block's
    body and the info string (the bit after [```] usually). *)

val to_info_string : t -> string
(** Convert the block back to the info string *)

val command_list : t -> string list
(** [command_list block] parses the [block]'s body for commands. *)

val alias : t -> string
(** A block's alias *)

val hash : t -> string option
(** If a block has been run it will hash a build hash *)

val kind : t -> kind
(** The kind of block *)

val body : t -> string
(** The body of the block *)
