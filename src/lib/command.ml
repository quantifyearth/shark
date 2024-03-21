open Astring

type t = { name : string; args : string list; file_args : string list }

let find_file_args args =
  (* gross liberties, we assume for now that any arg with a doubeldash might be a file. though ultimately this
     will have to rely on convention, annotation, or guesses, so it's not exactly that bad, just limited as is. I imagine
     we can have a common prefix for all files, like example.com should be used for domains. *)
  List.filter
    (fun arg ->
      let regex = Str.regexp "^/data" in
      Str.string_match regex arg 0)
    args

let parse_python_command args =
  let name =
    match args with
    | "-m" :: x :: _ -> x
    | x :: _ -> Filename.basename x
    | _ -> "Python script"
  in
  { name; args; file_args = find_file_args args }

let parse_rscript_command args =
  let r_options =
    [
      "--vanilla";
      "--save";
      "--no-environ";
      "--restore";
      "--no-site-file";
      "--no-init-file";
    ]
  in
  let rec strip_r_options args =
    match args with
    | [] -> []
    | hd :: tl -> (
        match List.mem hd r_options with
        | false -> args
        | true -> strip_r_options tl)
  in
  let reduced_args = strip_r_options args in
  let name =
    match reduced_args with x :: _ -> Filename.basename x | _ -> "R script"
  in
  { name; args; file_args = find_file_args args }

let parse_generic_commmand args =
  let name =
    match args with
    | "$" :: x :: _ -> x
    | x :: _ -> x
    | _ -> "Unrecognised command"
  in
  { name; args; file_args = find_file_args args }

let of_string command_str =
  (* one day this will be all generic, but for now I'm hardcoding python just to make progress *)
  match String.cuts ~sep:" " command_str with
  | [] -> None
  | "python3" :: args -> Some (parse_python_command args)
  | "Rscript" :: args -> Some (parse_rscript_command args)
  | name :: args -> Some (parse_generic_commmand (name :: args))

let name c = c.name
let file_args c = c.file_args
