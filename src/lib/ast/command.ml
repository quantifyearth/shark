open Astring
open Sexplib.Conv

type path = Fpath.t

let path_of_sexp = function
  | Ppx_sexp_conv_lib.Sexp.Atom s -> Fpath.v s
  | List _ -> Fpath.v ""

let sexp_of_path v = Ppx_sexp_conv_lib.Sexp.Atom (Fpath.to_string v)

type t = { name : string; args : string list; file_args : path list }
[@@deriving sexp]

let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
let v ~name ~args ~file_args = { name; args; file_args }
let raw_args c = c.args
let magic_path_regex = Str.regexp "^/data"

let find_file_args args =
  (* gross liberties, we assume for now that any arg with a doubeldash might be a file. though ultimately this
     will have to rely on convention, annotation, or guesses, so it's not exactly that bad, just limited as is. I imagine
     we can have a common prefix for all files, like example.com should be used for domains. *)
  List.filter_map
    (fun arg ->
      match Str.string_match magic_path_regex arg 0 with
      | false -> None
      | true -> (
          match Fpath.of_string arg with Error _e -> None | Ok r -> Some r))
    args

let parse_python_command args =
  let name =
    match args with
    | "-m" :: x :: _ -> x
    | x :: _ -> Filename.basename x
    | _ -> "Python script"
  in
  { name; args = "python3" :: args; file_args = find_file_args args }

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
  { name; args = "Rscript" :: args; file_args = find_file_args args }

let parse_generic_commmand args =
  let name =
    match args with
    | "$" :: x :: _ -> x
    | x :: _ -> x
    | _ -> "Unrecognised command"
  in
  { name; args; file_args = find_file_args args }

let of_string command_str =
  (* TODO: use a CLI parsing library for this *)
  let parts =
    String.cuts ~sep:" " command_str
    |> List.filter_map (fun x ->
           match String.length x with 0 -> None | _ -> Some x)
    |> List.map String.trim
  in
  let fparts = match parts with "$" :: x -> x | x -> x in
  match fparts with
  | [] -> None
  | "python3" :: args -> Some (parse_python_command args)
  | "Rscript" :: args -> Some (parse_rscript_command args)
  | name :: args -> Some (parse_generic_commmand (name :: args))

let name c = c.name
let file_args c : Fpath.t list = c.file_args
let to_string c = String.concat ~sep:" " c.args
