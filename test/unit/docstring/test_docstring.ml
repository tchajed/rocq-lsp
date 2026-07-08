(*************************************************************************)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Unit tests for Coq.Docstring                                          *)
(*************************************************************************)

let fail = ref false

let check name expected got =
  let show = function
    | None -> "None"
    | Some s -> Printf.sprintf "Some %S" s
  in
  if expected <> got then (
    fail := true;
    Printf.printf "FAIL %s: expected %s, got %s\n" name (show expected)
      (show got))

(* offset of the first occurrence of [marker] in [text] *)
let off text marker =
  let n = String.length marker in
  let rec go i =
    if i + n > String.length text then
      failwith ("marker not found: " ^ marker)
    else if String.equal (String.sub text i n) marker then i
    else go (i + 1)
  in
  go 0

let t name text marker expected =
  let offset = off text marker in
  check name expected (Coq.Docstring.find ~text ~offset)

let () =
  t "basic doc comment"
    "(** maps a function over a list *)\nDefinition map (f : A -> B) := x."
    "map ("
    (Some "maps a function over a list");

  t "plain comment is not a docstring"
    "(* internal helper *)\nDefinition help := x." "help" None;

  t "no comment at all" "Definition lonely := x." "lonely" None;

  t "doc comment separated by another sentence"
    "(** docs for foo *)\nDefinition foo := 1.\nDefinition bar := 2." "bar"
    None;

  t "multiline doc"
    "(** line one\n    line two *)\nFixpoint f (x : nat) := x." "f ("
    (Some "line one\n    line two");

  t "nested comment" "(** outer (* inner *) end *)\nDefinition n := 1." "n :="
    (Some "outer (* inner *) end");

  t "string with comment-closer in code before"
    "Definition s := \"*)\".\n(** real docs *)\nDefinition g := 1." "g :="
    (Some "real docs");

  t "string inside comment"
    "(** has \"*)\" inside string *)\nDefinition h := 1." "h :="
    (Some "has \"*)\" inside string");

  t "escaped quote in string"
    "Definition e := \"a\"\"(*\".\n(** docs e2 *)\nDefinition e2 := 1."
    "e2 :=" (Some "docs e2");

  t "plain comment between doc and def"
    "(** the docs *)\n(* impl note *)\nDefinition p := 1." "p :="
    (Some "the docs");

  t "decorative separator is not doc" "(*************)\nDefinition d := 1."
    "d :=" None;

  t "decorative separator between doc and def"
    "(** real *)\n(*****)\nDefinition d2 := 1." "d2 :=" (Some "real");

  t "blank lines between doc and def are ok"
    "(** spaced docs *)\n\n\nDefinition sp := 1." "sp :=" (Some "spaced docs");

  t "offset at identifier, not sentence start"
    "(** id docs *)\nProgram Definition ident := 1." "ident" (Some "id docs");

  t "attribute between comment and def"
    "(** attr docs *)\n#[global] Instance inst : C := {}." "inst"
    (Some "attr docs");

  t "unterminated comment before offset"
    "(* open comment\nDefinition u := 1." "u :=" None;

  (* to_markdown *)
  let m name input expected =
    check name (Some expected) (Some (Coq.Docstring.to_markdown input))
  in
  m "inline code" "applies [f] to [x]" "applies `f` to `x`";
  m "nested brackets" "see [fun x => [x]] here" "see `fun x => [x]` here";
  m "unmatched bracket" "a [ b" "a [ b";
  m "empty brackets" "a [] b" "a  b";

  if !fail then exit 1 else print_endline "all docstring tests passed"
