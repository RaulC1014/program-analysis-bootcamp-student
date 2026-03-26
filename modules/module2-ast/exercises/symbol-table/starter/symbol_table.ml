(* Symbol table with nested lexical scopes.

   Internal representation
   -----------------------
   We use a list of maps as a scope stack.  The head of the list is the
   innermost (most local) scope, and the tail holds enclosing scopes all
   the way out to the global scope.

   Each scope is a [StringMap.t] mapping identifier names to [symbol_info]
   records.
*)

module StringMap = Map.Make (String)

type symbol_info = {
  sym_name : string;
  sym_type : string;
  mutable_flag : bool;
}

(* The type [t] is a scope stack: a list of maps from names to symbol_info.
   The head of the list is the innermost scope. *)
type t = symbol_info StringMap.t list

let create () : t =
  [StringMap.empty]

let define (tbl : t) (name : string) (info : symbol_info) : t =
  match tbl with
  | [] ->
      [StringMap.add name info StringMap.empty]
  | scope :: rest ->
      StringMap.add name info scope :: rest

let lookup (tbl : t) (name : string) : symbol_info option =
  let rec aux scopes =
    match scopes with
    | [] -> None
    | scope :: rest ->
        (match StringMap.find_opt name scope with
         | Some info -> Some info
         | None -> aux rest)
  in
  aux tbl

let enter_scope (tbl : t) : t =
  StringMap.empty :: tbl

let exit_scope (tbl : t) : t option =
  match tbl with
  | [] -> None
  | [_] -> None
  | _scope :: rest -> Some rest
