(* ast_visualizer.ml - AST Structure Mapping Exercise
   ===================================================
   Implement three ways to visualize an AST built from Shared_ast.Ast_types:

   1. dump_ast       - Indented tree representation showing node types
   2. count_node_types - Count occurrences of each AST node type
   3. print_tree     - Pretty-print as human-readable pseudo-code

   Each function operates on a stmt list (i.e., a function body).
   Refer to Ast_types for the type definitions:
     expr: IntLit, BoolLit, Var, BinOp, UnaryOp, Call
     stmt: Assign, If, While, Return, Print, Block
*)

open Shared_ast.Ast_types

(* ------------------------------------------------------------------ *)
(* Helper: create an indentation string of [n] levels (2 spaces each) *)
(* ------------------------------------------------------------------ *)
let indent n = String.make (n * 2) ' '

(* ================================================================== *)
(* 1. dump_ast : stmt list -> string                                  *)
(*                                                                    *)
(*    Produce an indented, tree-style dump of the AST.                *)
(*    Example output for  Assign("x", IntLit 5):                      *)
(*      Assign("x")                                                   *)
(*        IntLit(5)                                                   *)
(*                                                                    *)
(*    For BinOp(Add, IntLit 2, IntLit 3):                             *)
(*      BinOp(+)                                                      *)
(*        IntLit(2)                                                   *)
(*        IntLit(3)                                                   *)
(* ================================================================== *)

(* Dump a single expression at the given indentation depth.
   Hint: use [indent depth] for the padding string, then match on the
   expression and format each variant.  For compound nodes (BinOp, UnaryOp,
   Call), recursively dump the sub-expressions at [depth + 1]. *)
let rec dump_expr (depth : int) (e : expr) : string =
  let pad = indent depth in
  match e with
  | IntLit n ->
    Printf.sprintf "%sIntLit(%d)" pad n
  | BoolLit b ->
    Printf.sprintf "%sBoolLit(%b)" pad b
  | Var s ->
    Printf.sprintf "%sVar(\"%s\")" pad s
  | BinOp (op, e1, e2) ->
    Printf.sprintf "%sBinOp(%s)\n%s\n%s"
      pad
      (match op with
       | Add -> "+"
       | Sub -> "-"
       | Mul -> "*"
       | Div -> "/"
       | Eq  -> "=="
       | Neq -> "!="
       | Lt  -> "<"
       | Gt  -> ">"
       | Le  -> "<="
       | Ge  -> ">="
       | And -> "&&"
       | Or  -> "||")
      (dump_expr (depth + 1) e1)
      (dump_expr (depth + 1) e2)
  | UnaryOp (op, e1) ->
    Printf.sprintf "%sUnaryOp(%s)\n%s"
      pad
      (match op with
       | Neg -> "-"
       | Not -> "!")
      (dump_expr (depth + 1) e1)
  | Call (name, args) ->
    let dumped_args = List.map (dump_expr (depth + 1)) args |> String.concat "\n" in
    if dumped_args = "" then
      Printf.sprintf "%sCall(\"%s\")" pad name
    else
      Printf.sprintf "%sCall(\"%s\")\n%s" pad name dumped_args

(* Dump a single statement at the given indentation depth.
   For statements that contain expressions (Assign, If, While, Return, Print),
   use dump_expr at [depth + 1].
   For statements that contain sub-statement lists (If, While, Block),
   use dump_stmts at [depth + 1]. *)
and dump_stmt (depth : int) (s : stmt) : string =
  let pad = indent depth in
  match s with
  | Assign (v, e) ->
    Printf.sprintf "%sAssign(\"%s\")\n%s" pad v (dump_expr (depth + 1) e)
  | If (cond, then_b, else_b) ->
    Printf.sprintf "%sIf\n%s\n%sThen\n%s\n%sElse\n%s"
      pad
      (dump_expr (depth + 1) cond)
      pad
      (dump_stmts (depth + 1) then_b)
      pad
      (dump_stmts (depth + 1) else_b)
  | While (cond, body) ->
    Printf.sprintf "%sWhile\n%s\n%sBody\n%s"
      pad
      (dump_expr (depth + 1) cond)
      pad
      (dump_stmts (depth + 1) body)
  | Return None ->
    Printf.sprintf "%sReturn()" pad
  | Return (Some e) ->
    Printf.sprintf "%sReturn\n%s" pad (dump_expr (depth + 1) e)
  | Print exprs ->
    let dumped_exprs = List.map (dump_expr (depth + 1)) exprs |> String.concat "\n" in
    if dumped_exprs = "" then
      Printf.sprintf "%sPrint" pad
    else
      Printf.sprintf "%sPrint\n%s" pad dumped_exprs
  | Block stmts ->
    let dumped_stmts = dump_stmts (depth + 1) stmts in
    if dumped_stmts = "" then
      Printf.sprintf "%sBlock" pad
    else
      Printf.sprintf "%sBlock\n%s" pad dumped_stmts

(* Dump a list of statements, joining them with newlines. *)
and dump_stmts (depth : int) (stmts : stmt list) : string =
  List.map (dump_stmt depth) stmts |> String.concat "\n"

(* Top-level entry point: dump an entire function body. *)
let dump_ast (stmts : stmt list) : string =
  dump_stmts 0 stmts


(* ================================================================== *)
(* 2. count_node_types : stmt list -> (string * int) list             *)
(*                                                                    *)
(*    Walk the AST and count how many of each node type appear.       *)
(*    Return an association list like:                                 *)
(*      [("Assign", 2); ("BinOp", 3); ("IntLit", 4); ...]            *)
(*                                                                    *)
(*    Node type names to count:                                       *)
(*      Statements: "Assign", "If", "While", "Return", "Print",      *)
(*                  "Block"                                           *)
(*      Expressions: "IntLit", "BoolLit", "Var", "BinOp",            *)
(*                   "UnaryOp", "Call"                                *)
(* ================================================================== *)

(* Helper: increment the count for [key] in an association list.
   If the key is not present, add it with count 1. *)
let inc (key : string) (counts : (string * int) list) : (string * int) list =
  let rec aux = function
    | [] -> [(key, 1)]
    | (k, v) :: rest when k = key -> (k, v + 1) :: rest
    | pair :: rest -> pair :: aux rest
  in
  aux counts

(* Count node types inside an expression, accumulating into [acc].
   Match on each expr variant, use [inc] to add its type name, then
   recurse into any sub-expressions. *)
let rec count_expr (acc : (string * int) list) (e : expr) : (string * int) list =
  match e with
  | IntLit _ ->
    inc "IntLit" acc
  | BoolLit _ ->
    inc "BoolLit" acc
  | Var _ ->
    inc "Var" acc
  | BinOp (_, e1, e2) ->
    let acc1 = inc "BinOp" acc in
    let acc2 = count_expr acc1 e1 in
    count_expr acc2 e2
  | UnaryOp (_, e1) ->
    let acc1 = inc "UnaryOp" acc in
    count_expr acc1 e1
  | Call (_, args) ->
    let acc1 = inc "Call" acc in
    List.fold_left count_expr acc1 args

(* Count node types inside a statement, accumulating into [acc]. *)
and count_stmt (acc : (string * int) list) (s : stmt) : (string * int) list =
  match s with
  | Assign (_, e) ->
    let acc1 = inc "Assign" acc in
    count_expr acc1 e
  | If (cond, then_b, else_b) ->
    let acc1 = inc "If" acc in
    let acc2 = count_expr acc1 cond in
    let acc3 = count_stmts acc2 then_b in
    count_stmts acc3 else_b
  | While (cond, body) ->
    let acc1 = inc "While" acc in
    let acc2 = count_expr acc1 cond in
    count_stmts acc2 body
  | Return None ->
    inc "Return" acc
  | Return (Some e) ->
    let acc1 = inc "Return" acc in
    count_expr acc1 e
  | Print exprs ->
    let acc1 = inc "Print" acc in
    List.fold_left count_expr acc1 exprs
  | Block stmts ->
    let acc1 = inc "Block" acc in
    count_stmts acc1 stmts

(* Count across a list of statements. *)
and count_stmts (acc : (string * int) list) (stmts : stmt list) : (string * int) list =
  List.fold_left count_stmt acc stmts

(* Top-level entry point. *)
let count_node_types (stmts : stmt list) : (string * int) list =
  count_stmts [] stmts


(* ================================================================== *)
(* 3. print_tree : stmt list -> string                                *)
(*                                                                    *)
(*    Pretty-print the AST as human-readable pseudo-code.             *)
(*    Example:                                                        *)
(*      x = (2 + 3);                                                  *)
(*      if ((x > 0)) {                                                *)
(*        y = 1;                                                      *)
(*      } else {                                                      *)
(*        y = 0;                                                      *)
(*      }                                                             *)
(* ================================================================== *)

(* Convert an operator to its string symbol.
   Add -> "+", Sub -> "-", Mul -> "*", Div -> "/",
   Eq -> "==", Neq -> "!=", Lt -> "<", Gt -> ">",
   Le -> "<=", Ge -> ">=", And -> "&&", Or -> "||" *)
let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Gt  -> ">"
  | Le  -> "<="
  | Ge  -> ">="
  | And -> "&&"
  | Or  -> "||"

(* Convert a unary operator to its string symbol. *)
let string_of_uop (uop : uop) : string =
  match uop with
  | Neg -> "-"
  | Not -> "!"

(* Convert an expression to a string (parenthesized where needed).
     - IntLit n   -> string_of_int n
     - BoolLit b  -> string_of_bool b
     - Var s      -> s
     - BinOp      -> "(<left> <op> <right>)"
     - UnaryOp    -> "(<op><expr>)"
     - Call       -> "<name>(<arg1>, <arg2>, ...)" *)
let rec expr_to_string (e : expr) : string =
  match e with
  | IntLit n ->
    string_of_int n
  | BoolLit b ->
    string_of_bool b
  | Var s ->
    s
  | BinOp (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (expr_to_string e1)
      (string_of_op op)
      (expr_to_string e2)
  | UnaryOp (uop, e1) ->
    Printf.sprintf "(%s%s)"
      (string_of_uop uop)
      (expr_to_string e1)
  | Call (name, args) ->
    Printf.sprintf "%s(%s)"
      name
      (List.map expr_to_string args |> String.concat ", ")

(* Pretty-print a single statement at the given indentation level.
     - Assign: "<pad><var> = <expr>;"
     - If:     "<pad>if (<cond>) {\n<then>\n<pad>} else {\n<else>\n<pad>}"
               (omit else clause if the else branch is [])
     - While:  "<pad>while (<cond>) {\n<body>\n<pad>}"
     - Return None:    "<pad>return;"
     - Return Some e:  "<pad>return <expr>;"
     - Print:  "<pad>print(<expr1>, <expr2>, ...);"
     - Block:  "<pad>{\n<stmts>\n<pad>}" *)
and pp_stmt (depth : int) (s : stmt) : string =
  let pad = indent depth in
  match s with
  | Assign (v, e) ->
    Printf.sprintf "%s%s = %s;" pad v (expr_to_string e)
  | If (cond, then_b, else_b) ->
    let then_part = pp_stmts (depth + 1) then_b in
    if else_b = [] then
      Printf.sprintf "%sif (%s) {\n%s\n%s}"
        pad
        (expr_to_string cond)
        then_part
        pad
    else
      let else_part = pp_stmts (depth + 1) else_b in
      Printf.sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}"
        pad
        (expr_to_string cond)
        then_part
        pad
        else_part
        pad
  | While (cond, body) ->
    Printf.sprintf "%swhile (%s) {\n%s\n%s}"
      pad
      (expr_to_string cond)
      (pp_stmts (depth + 1) body)
      pad
  | Return None ->
    Printf.sprintf "%sreturn;" pad
  | Return (Some e) ->
    Printf.sprintf "%sreturn %s;" pad (expr_to_string e)
  | Print exprs ->
    Printf.sprintf "%sprint(%s);"
      pad
      (List.map expr_to_string exprs |> String.concat ", ")
  | Block stmts ->
    Printf.sprintf "%s{\n%s\n%s}"
      pad
      (pp_stmts (depth + 1) stmts)
      pad

(* Pretty-print a list of statements, joining with newlines. *)
and pp_stmts (depth : int) (stmts : stmt list) : string =
  List.map (pp_stmt depth) stmts |> String.concat "\n"

(* Top-level entry point. *)
let print_tree (stmts : stmt list) : string =
  pp_stmts 0 stmts


(* ================================================================== *)
(* Main - run the visualizer on the sample programs                   *)
(* ================================================================== *)
let () =
  let open Sample_asts in
  let programs = [
    ("simple_arithmetic", simple_arithmetic);
    ("branching",         branching);
    ("loop_example",      loop_example);
  ] in
  List.iter (fun (label, prog) ->
    let body = (List.hd prog).body in
    Printf.printf "=== %s ===\n\n" label;

    Printf.printf "--- dump_ast ---\n%s\n\n" (dump_ast body);

    Printf.printf "--- count_node_types ---\n";
    let counts = count_node_types body in
    List.iter (fun (name, n) ->
      Printf.printf "  %s: %d\n" name n
    ) counts;
    print_newline ();

    Printf.printf "--- print_tree ---\n%s\n\n" (print_tree body);
  ) programs
