(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Docstring extraction                   *)
(*************************************************************************)

(* Span of a comment in the text, delimiters included: [start, stop) *)
type comment =
  { start : int
  ; stop : int
  }

(* Scan [text] up to offset [limit], returning all comments closed before
   [limit], most recent first. Comments nest, and string literals are
   respected both in code and inside comments, following Rocq's lexer. *)
let scan text limit =
  let n = min (String.length text) limit in
  let at i c = i < n && text.[i] = c in
  let rec code i acc =
    if i >= n then acc
    else if at i '(' && at (i + 1) '*' then comment (i + 2) 1 i acc
    else if at i '"' then string_ (i + 1) (fun j -> code j acc) acc
    else code (i + 1) acc
  and comment i depth start acc =
    if i >= n then acc (* comment still open at [limit] *)
    else if at i '(' && at (i + 1) '*' then comment (i + 2) (depth + 1) start acc
    else if at i '*' && at (i + 1) ')' then
      if depth = 1 then code (i + 2) ({ start; stop = i + 2 } :: acc)
      else comment (i + 2) (depth - 1) start acc
    else if at i '"' then
      string_ (i + 1) (fun j -> comment j depth start acc) acc
    else comment (i + 1) depth start acc
  and string_ i k acc =
    if i >= n then acc
    else if at i '"' then
      (* [""] is an escaped quote inside a string literal *)
      if at (i + 1) '"' then string_ (i + 2) k acc else k (i + 1)
    else string_ (i + 1) k acc
  in
  code 0 []

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

(* A coqdoc comment starts with "(**"; "(***..." is a decorative separator *)
let is_doc text { start; stop } =
  stop - start > 4 && text.[start + 2] = '*' && text.[start + 3] <> '*'

let only_ws text from until =
  let rec go i = i >= until || (is_ws text.[i] && go (i + 1)) in
  go from

(* The gap between a docstring and its definition must stay within one
   sentence: no '.' followed by whitespace *)
let no_sentence_end text from until =
  let rec go i =
    if i >= until then true
    else if
      text.[i] = '.'
      && (i + 1 >= String.length text || is_ws text.[i + 1])
    then false
    else go (i + 1)
  in
  go from

(* Contents of a comment, delimiters stripped *)
let contents text { start; stop } =
  let start = start + 3 (* "(**" *) in
  let stop = stop - 2 (* "*)" *) in
  if stop <= start then "" else String.trim (String.sub text start (stop - start))

let find ~text ~offset =
  match scan text offset with
  | [] -> None
  | last :: prev ->
    if not (no_sentence_end text last.stop offset) then None
    else
      (* Walk back over plain comments directly attached to the doc one *)
      let rec pick c prev =
        if is_doc text c then Some (contents text c)
        else
          match prev with
          | p :: rest when only_ws text p.stop c.start -> pick p rest
          | _ -> None
      in
      pick last prev

(* Translate coqdoc inline code, [id], to markdown backquotes. Brackets nest
   in coqdoc, e.g. [fun x => [x]] *)
let to_markdown s =
  let n = String.length s in
  let b = Buffer.create n in
  let matching i =
    let rec go i depth =
      if i >= n then None
      else
        match s.[i] with
        | '[' -> go (i + 1) (depth + 1)
        | ']' -> if depth = 1 then Some i else go (i + 1) (depth - 1)
        | _ -> go (i + 1) depth
    in
    go i 1
  in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '[' then (
      match matching (i + 1) with
      | Some j when j > i + 1 ->
        Buffer.add_char b '`';
        Buffer.add_string b (String.sub s (i + 1) (j - i - 1));
        Buffer.add_char b '`';
        go (j + 1)
      | Some j (* empty [] *) -> go (j + 1)
      | None ->
        Buffer.add_char b '[';
        go (i + 1))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b
