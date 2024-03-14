open Lwt.Infix
open Sexplib0.Sexp_conv

module Block = struct
  type t = {
    kind : [ `Build | `Run ];
    hash : string option;
    alias : string;
    body : string;
  }
  [@@deriving sexp]

  let of_info_string ~body s =
    match Astring.String.cut ~sep:":" s with
    | Some ("shark-build", env) ->
        Some { kind = `Build; hash = None; alias = env; body }
    | Some ("shark-run", env) ->
        Some { kind = `Run; hash = None; alias = env; body }
    | _ -> None

  let to_info_string t =
    match t.kind with
    | `Build -> (
        Fmt.str "shark-build:%s" t.alias
        ^ match t.hash with Some hash -> ":" ^ hash | None -> "")
    | `Run -> (
        Fmt.str "shark-run:%s" t.alias
        ^ match t.hash with Some hash -> hash | None -> "")

  let pp ppf v = Sexplib.Sexp.pp_hum ppf (sexp_of_t v)
end

let map_blocks (doc : Cmarkit.Doc.t) fn =
  let alias_hash_map = ref [] in
  let block _mapper = function
    | Cmarkit.Block.Code_block (node, meta) -> (
        match Cmarkit.Block.Code_block.info_string node with
        | None -> `Default
        | Some (s, _) -> (
            let body = Cmarkit.Block.Code_block.code node in
            let body =
              List.map Cmarkit.Block_line.to_string body |> String.concat "\n"
            in
            match Block.of_info_string ~body s with
            | Some block ->
                let new_block = fn alias_hash_map node block in
                `Map (Some (Cmarkit.Block.Code_block (new_block, meta)))
            | None -> `Default))
    | _ -> `Default
  in
  let mapper = Cmarkit.Mapper.make ~block () in
  Cmarkit.Mapper.map_doc mapper doc

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Sandbox = Obuilder.Sandbox
module Fetcher = Obuilder.Docker
module Store_spec = Obuilder.Store_spec

let ( / ) = Filename.concat

let create_builder spec conf =
  let open Obuilder in
  let (Store_spec.Store ((module Store), store)) =
    Lwt_eio.Promise.await_lwt spec
  in
  let module Builder = Obuilder.Builder (Store) (Sandbox) (Fetcher) in
  Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let log buffer tag msg =
  match tag with
  | `Heading -> ()
  | `Note -> ()
  | `Output -> Buffer.add_string buffer msg

let process_block alias_hash_map store conf (code_block, block) =
  create_builder store conf >>= fun (Builder ((module Builder), builder)) ->
  Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ fun () ->
  match block.Block.kind with
  | `Build -> (
      let spec = Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string block.body) in
      let buf = Buffer.create 128 in
      let log = log buf in
      let context = Obuilder.Context.v ~log ~src_dir:"." () in
      Builder.build builder context spec >>= function
      | Error `Cancelled -> failwith "Cancelled by user"
      | Error (`Msg m) -> failwith m
      | Ok id ->
          let block = { block with hash = Some id } in
          let new_code_block =
            let info_string = Block.to_info_string block in
            Cmarkit.Block.Code_block.make
              ~info_string:(info_string, Cmarkit.Meta.none)
              (Cmarkit.Block.Code_block.code code_block)
          in
          Lwt.return (new_code_block, block))
  | `Run ->
      let commands =
        String.split_on_char '\n' block.body
        |> List.filter (String.starts_with ~prefix:"$ ")
      in
      let commands_stripped =
        List.map (fun s -> String.sub s 1 (String.length s - 1)) commands
      in
      let build = List.assoc block.alias alias_hash_map in
      let spec build_hash command =
        let open Obuilder_spec in
        stage ~from:(`Build build_hash) [ run "%s" command ]
      in
      let process (outputs, build_hash) command =
        Logs.info (fun f ->
            f "Running spec %a" Obuilder_spec.pp (spec build_hash command));
        let buf = Buffer.create 128 in
        let log = log buf in
        let context = Obuilder.Context.v ~log ~src_dir:"." () in
        Builder.build builder context (spec build_hash command) >>= function
        | Ok id -> Lwt.return ((id, Buffer.contents buf) :: outputs, id)
        | Error _ -> failwith "Procressing failed"
      in
      Lwt_list.fold_left_s process ([], build) commands_stripped
      >>= fun (ids_and_output, _hash) ->
      let ids_and_output = List.rev ids_and_output in
      let id = List.hd ids_and_output |> fst in
      let body =
        List.fold_left
          (fun s (command, (_, output)) -> s @ [ command; output ])
          []
          (List.combine commands ids_and_output)
        |> List.filter (fun v -> not (String.equal "" v))
        |> List.map Cmarkit.Block_line.list_of_string
        |> List.concat
      in
      let info_string = Cmarkit.Block.Code_block.info_string code_block in
      Lwt.return
        ( Cmarkit.Block.Code_block.make ?info_string body,
          { block with hash = Some id } )
