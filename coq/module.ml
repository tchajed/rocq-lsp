(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq module API                        *)
(*************************************************************************)

type t =
  { dp : Names.DirPath.t
  ; source : string
  ; vo : string
  ; uri : Lang.LUri.File.t
  }

let uri { uri; _ } = uri
let source { source; _ } = source

let make dp =
  match Loadpath.locate_absolute_library dp with
  | Ok vo ->
    (* Fleche.Io.Log.trace "rq_definition" "File Found"; *)
    let source = Filename.remove_extension vo ^ ".v" in
    let source = Str.replace_first (Str.regexp "_build/default/") "" source in
    let uri = Lang.LUri.of_string ("file://" ^ source) in
    let uri = Lang.LUri.File.of_uri uri |> Result.get_ok in
    Ok { dp; source; vo; uri }
  | Error err ->
    (* Fleche.Io.Log.trace "rq_definition" "File Not Found :("; *)
    (* Debug? *)
    Error err

let offset_to_range source (bp, ep) =
  let text = Compat.Ocaml_414.In_channel.(with_open_text source input_all) in
  let rec count (lines, char) cur goal =
    if cur >= goal then (lines, char)
    else
      match text.[cur] with
      | '\n' -> count (lines + 1, 0) (cur + 1) goal
      | _ -> count (lines, char + 1) (cur + 1) goal
  in
  (* XXX UTF-8 / 16 adjust *)
  let bline, bchar = count (0, 0) 0 bp in
  let eline, echar = count (bline, bchar) bp ep in
  let start = Lang.Point.{ line = bline; character = bchar; offset = bp } in
  let end_ = Lang.Point.{ line = eline; character = echar; offset = ep } in
  Lang.Range.{ start; end_ }

let with_info { vo; _ } name ~f =
  let glob = Filename.remove_extension vo ^ ".glob" in
  match Glob.open_file glob with
  | Error err -> Error err
  | Ok g -> Ok (Option.map f (Glob.get_info g name))

let base_name name =
  match String.rindex_opt name '.' with
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)
  | None -> name

(* Check the definition is really at [offset]; guards against a stale .glob
   file whose offsets don't match the current source *)
let check_id text offset id =
  let n = String.length id in
  offset >= 0
  && offset + n <= String.length text
  && String.equal (String.sub text offset n) id

let docstring mod_ name =
  let f { Glob.Info.offset = bp, _; _ } =
    let text =
      Compat.Ocaml_414.In_channel.(with_open_text mod_.source input_all)
    in
    if check_id text bp (base_name name) then Docstring.find ~text ~offset:bp
    else None
  in
  match with_info mod_ name ~f with
  | Error err -> Error err
  | Ok (Some doc) -> Ok doc
  | Ok None -> Ok None

let find { vo; source; _ } name =
  let glob = Filename.remove_extension vo ^ ".glob" in
  match Glob.open_file glob with
  | Error err ->
    (* Fleche.Io.Log.trace "rq_definition:open_file" "Error: %s" err; *)
    Error err
  | Ok g -> (
    match Glob.get_info g name with
    | Some { offset; _ } -> Ok (Some (offset_to_range source offset))
    | None ->
      (* Fleche.Io.Log.trace "rq_definition:get_info" "Not found"; *)
      Ok None)
