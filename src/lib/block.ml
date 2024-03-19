open Astring

module Block = struct
  type t = {
    kind : [ `Build | `Run ];
    hash : string option;
    alias : string;
    body : string;
  }

  let of_info_string body info =
    match String.cut ~sep:":" info with
    | Some ("shark-build", env) ->
        Some { kind = `Build; hash = None; alias = env; body }
    | Some ("shark-run", env) ->
        Some { kind = `Run; hash = None; alias = env; body }
    | None -> Some { kind = `Run; hash = None; alias = info; body }
    | _ -> None

  let body b = b.body

  let command_list b =
    let regex_newline = Str.regexp "\\\\\n"
    and regex_comment = Str.regexp "#.*$"
    and regex_whitespace = Str.regexp "[\t ]+" in
    Str.global_replace regex_newline "" b.body
    |> Str.global_replace regex_comment ""
    |> String.cuts ~sep:"\n" |> List.map String.trim
    |> List.map (Str.global_replace regex_whitespace " ")
    |> List.filter_map (fun l -> match l with "" -> None | x -> Some x)
end
