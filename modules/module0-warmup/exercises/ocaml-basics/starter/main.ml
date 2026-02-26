(* ================================================================
   Exercise 1: OCaml Basics -- "Token Classifier"
   ================================================================

   Theme: You are building helper functions for a simple lexer that
   classifies characters in source code. This previews how real
   lexers (Module 2, Lab 2) break source text into tokens.

   Concepts: let bindings, type annotations, if/then/else,
   pattern matching, String operations, tuples, Printf.

   Run with:  dune exec modules/module0-warmup/exercises/ocaml-basics/starter/main.exe
   ================================================================ *)

(* ----------------------------------------------------------------
   Part 1: Simple Functions
   ---------------------------------------------------------------- *)

(** [square x] returns x * x. *)
let square (_x : int) : int =
  _x * _x

(** [is_empty s] returns true if the string [s] has length 0. *)
let is_empty (_s : string) : bool =
  String.length _s = 0

(** [greet name] returns the string "Hello, <name>!". *)
let greet (_name : string) : string =
  Printf.sprintf "Hello, %s!" _name
  (* alternatively: "Hello, " ^ _name ^ "!" *)

(* ----------------------------------------------------------------
   Part 2: Character Classification

   In a real lexer, we classify each character to decide what kind
   of token it starts. Here we do it with simple if/then/else.
   ---------------------------------------------------------------- *)

(** [is_digit c] returns true if c is '0'..'9'. *)
let is_digit (_c : char) : bool =
  _c >= '0' && _c <= '9'
[@@warning "-32"]

(** [is_alpha c] returns true if c is 'a'..'z' or 'A'..'Z' or '_'. *)
let is_alpha (_c : char) : bool =
  (_c >= 'a' && _c <= 'z')
  || (_c >= 'A' && _c <= 'Z')
  || _c = '_'
[@@warning "-32"]

(** [classify_char c] returns a string describing the character:
    - "digit"    if c is '0'..'9'
    - "alpha"    if c is a letter or underscore
    - "operator" if c is one of '+', '-', '*', '/'
    - "unknown"  otherwise *)
let classify_char (_c : char) : string =
  if is_digit _c then "digit"
  else if is_alpha _c then "alpha"
  else
    match _c with
    | '+' | '-' | '*' | '/' -> "operator"
    | _ -> "unknown"

(* ----------------------------------------------------------------
   Part 3: Token Types and Formatting

   A token pairs a category with its text. We represent this as a
   tuple: (string * string). Example: ("keyword", "if")
   ---------------------------------------------------------------- *)

(** A token is a (category, text) pair. *)
type token = string * string

(** [format_token tok] returns a string like "[keyword: if]".
    Given token ("keyword", "if"), return "[keyword: if]". *)
let format_token ((_cat, _text) : token) : string =
  Printf.sprintf "[%s: %s]" _cat _text

(** [make_token text] creates a token by classifying the first
    character of [text]:
    - if [text] is empty, return ("empty", "")
    - if the first char is a digit, return ("number", text)
    - if the first char is alpha, return ("identifier", text)
    - otherwise return ("symbol", text) *)
let make_token (_text : string) : token =
  if String.length _text = 0 then ("empty", "")
  else
    let c = _text.[0] in
    if is_digit c then ("number", _text)
    else if is_alpha c then ("identifier", _text)
    else ("symbol", _text)

(* ----------------------------------------------------------------
   Part 4: Positions (Tuples)

   Lexers track source positions as (line, column) pairs.
   ---------------------------------------------------------------- *)

(** A position is a (line, column) pair. *)
type pos = int * int

(** [format_pos p] returns a string like "line 1, col 5". *)
let format_pos ((_line, _col) : pos) : string =
  Printf.sprintf "line %d, col %d" _line _col

(** [advance_pos p c] returns a new position after reading char [c]:
    - if c is '\n', move to the next line (line+1, col 1)
    - otherwise, stay on the same line (line, col+1) *)
let advance_pos ((_line, _col) : pos) (_c : char) : pos =
  if _c = '\n' then (_line + 1, 1)
  else (_line, _col + 1)

(** [scan_positions s] folds over the characters in [s], starting
    at position (1, 1), advancing with each character, and returns
    the final position. *)
let scan_positions (s : string) : pos =
  let len = String.length s in
  let rec go i ((line, col) : pos) : pos =
    if i >= len then (line, col)
    else go (i + 1) (advance_pos (line, col) s.[i])
  in
  go 0 (1, 1)

(* ================================================================
   Main -- runs all exercises and prints results.
   Compare your output against the STUDENT_README.
   ================================================================ *)
let () =
  Printf.printf "=== Exercise 1: OCaml Basics ===\n\n";

  (* Part 1 *)
  Printf.printf "square 5 = %d\n" (square 5);
  Printf.printf "square (-3) = %d\n" (square (-3));
  Printf.printf "is_empty \"\" = %b\n" (is_empty "");
  Printf.printf "is_empty \"hi\" = %b\n" (is_empty "hi");
  Printf.printf "greet \"OCaml\" = %s\n\n" (greet "OCaml");

  (* Part 2 *)
  Printf.printf "classify_char '7' = %s\n" (classify_char '7');
  Printf.printf "classify_char 'x' = %s\n" (classify_char 'x');
  Printf.printf "classify_char '+' = %s\n" (classify_char '+');
  Printf.printf "classify_char '!' = %s\n\n" (classify_char '!');

  (* Part 3 *)
  Printf.printf "format_token (\"keyword\", \"if\") = %s\n"
    (format_token ("keyword", "if"));
  Printf.printf "make_token \"42\" = %s\n"
    (format_token (make_token "42"));
  Printf.printf "make_token \"hello\" = %s\n"
    (format_token (make_token "hello"));
  Printf.printf "make_token \"+\" = %s\n"
    (format_token (make_token "+"));
  Printf.printf "make_token \"\" = %s\n\n"
    (format_token (make_token ""));

  (* Part 4 *)
  Printf.printf "format_pos (1, 1) = %s\n" (format_pos (1, 1));
  Printf.printf "advance_pos (1,1) 'a' = %s\n"
    (format_pos (advance_pos (1, 1) 'a'));
  Printf.printf "advance_pos (1,3) '\\n' = %s\n"
    (format_pos (advance_pos (1, 3) '\n'));
  Printf.printf "scan_positions \"ab\\ncd\" = %s\n"
    (format_pos (scan_positions "ab\ncd"));

  Printf.printf "\nDone!\n"