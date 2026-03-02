(* ================================================================
   Exercise 3: Collections and Records -- "Variable Tracker"
   ================================================================

   Theme: You are building a miniature variable tracker that
   monitors assignments in a program. This directly previews
   Module 3's reaching-definitions analysis, which tracks which
   assignments "reach" each program point.

   Concepts: List.map, List.filter, List.fold_left, records,
   Map.Make(String), Set.Make(String), ref (mutable state).

   Run with:  dune exec modules/module0-warmup/exercises/collections-and-records/starter/main.exe
   ================================================================ *)

(* ----------------------------------------------------------------
   Part 1: List Higher-Order Functions
   ---------------------------------------------------------------- *)

(** [double_all xs] returns a new list with every element doubled.
    Example: double_all [1; 2; 3] = [2; 4; 6] *)
let double_all (_xs : int list) : int list =
  List.map (fun x -> 2 * x) _xs

(** [keep_positive xs] returns only the positive elements.
    Example: keep_positive [-1; 3; 0; 5; -2] = [3; 5] *)
let keep_positive (_xs : int list) : int list =
  List.filter (fun x -> x > 0) _xs

(** [sum xs] returns the sum of all elements.
    Example: sum [1; 2; 3; 4] = 10 *)
let sum (_xs : int list) : int =
  List.fold_left(fun acc x -> acc + x) 0 _xs

(** [has_duplicates xs] returns true if any string appears more
    than once in [xs].
    Hint: sort first, then check adjacent elements. *)
let has_duplicates (xs : string list) : bool =
  let sorted = List.sort String.compare xs in
  let rec check = function
    | [] | [_] -> false
    | a :: (b :: _rest as tail) ->
      if a = b then true else check tail
  in
  check sorted

(* ----------------------------------------------------------------
   Part 2: Records

   In Module 3, analysis results are stored as records. Here is a
   simplified version representing a variable assignment.
   ---------------------------------------------------------------- *)

(** A record tracking one assignment statement. *)
type assignment = {
  var_name : string;  (** variable being assigned *)
  value    : int;     (** value assigned *)
  line     : int;     (** line number *)
}

(** [make_assign name value line] creates an assignment record. *)
let make_assign (_name : string) (_value : int) (_line : int) : assignment =
  { var_name = _name; value = _value; line = _line }

(** [format_assign a] returns "x = 5 (line 3)". *)
let format_assign (_a : assignment) : string =
  Printf.sprintf "%s = %d (line %d)" _a.var_name _a.value _a.line

(** [increment_value a n] returns a new record with value increased
    by [n]. Records are immutable -- use { a with ... } syntax. *)
let increment_value (_a : assignment) (_n : int) : assignment =
  { _a with value = _a.value + _n }

(* ----------------------------------------------------------------
   Part 3: StringMap -- Variable Environments

   In the bootcamp, variable environments map variable names to
   abstract values. Here we use a concrete int map.
   ---------------------------------------------------------------- *)

module StringMap = Map.Make(String)

(** [build_env pairs] creates a StringMap from a list of (name, value)
    pairs. Later pairs overwrite earlier ones if names conflict.

    Example: build_env [("x", 1); ("y", 2)] builds {x->1, y->2} *)
let build_env (_pairs : (string * int) list) : int StringMap.t =
  List.fold_left
    (fun env (name, value) -> StringMap.add name value env)
    StringMap.empty
    _pairs
  

(** [lookup_var env name] returns Some value if [name] is in [env],
    or None otherwise. *)
let lookup_var (_env : int StringMap.t) (_name : string) : int option =
  StringMap.find_opt _name _env

(** [all_vars env] returns a sorted list of all variable names in
    the environment.

    Hint: StringMap.bindings returns a (key * value) list. *)
let all_vars (_env : int StringMap.t) : string list =
  StringMap.bindings _env
  |> List.map fst

(* ----------------------------------------------------------------
   Part 4: StringSet -- Tracking Variable Sets

   Dataflow analyses in Modules 3-5 use sets to track which
   variables are live, defined, or tainted at each program point.
   ---------------------------------------------------------------- *)

module StringSet = Set.Make(String)

(** [assigned_vars assignments] returns a StringSet of all variable
    names that appear in the assignment list. *)
let assigned_vars (_assignments : assignment list) : StringSet.t =
  List.fold_left
    (fun set a -> StringSet.add a.var_name set)
    StringSet.empty
    _assignments

(** [common_vars s1 s2] returns the intersection of two StringSets. *)
let common_vars (_s1 : StringSet.t) (_s2 : StringSet.t) : StringSet.t =
  StringSet.inter _s1 _s2

(* ----------------------------------------------------------------
   Part 5: Mutable State (ref)

   OCaml is primarily functional, but sometimes we need mutable
   state. The [ref] type is a mutable cell -- you will see it
   used in fixpoint loops (Module 3-4).
   ---------------------------------------------------------------- *)

(** [make_counter ()] returns a function that returns a fresh
    integer each time it is called: 0, 1, 2, 3, ...

    Example:
      let next = make_counter () in
      next ()  (* 0 *)
      next ()  (* 1 *)
      next ()  (* 2 *)

    Hint: create a ref inside make_counter and return a closure
    that increments and returns its value. *)
let make_counter () : unit -> int =
  let r = ref 0 in
  fun () ->
    let v  = !r in
    r := v + 1;
    v

(* ================================================================
   Main -- runs all exercises and prints results.
   ================================================================ *)
let () =
  Printf.printf "=== Exercise 3: Collections and Records ===\n\n";

  (* Part 1: Lists *)
  let show_ints xs =
    "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"
  in
  Printf.printf "double_all [1; 2; 3] = %s\n"
    (show_ints (double_all [1; 2; 3]));
  Printf.printf "keep_positive [-1; 3; 0; 5; -2] = %s\n"
    (show_ints (keep_positive [-1; 3; 0; 5; -2]));
  Printf.printf "sum [1; 2; 3; 4] = %d\n" (sum [1; 2; 3; 4]);
  Printf.printf "has_duplicates [\"a\"; \"b\"; \"a\"] = %b\n"
    (has_duplicates ["a"; "b"; "a"]);
  Printf.printf "has_duplicates [\"a\"; \"b\"; \"c\"] = %b\n\n"
    (has_duplicates ["a"; "b"; "c"]);

  (* Part 2: Records *)
  let a1 = make_assign "x" 5 1 in
  let a2 = make_assign "y" 10 2 in
  let a3 = make_assign "x" 7 3 in
  Printf.printf "format_assign a1 = %s\n" (format_assign a1);
  Printf.printf "format_assign a2 = %s\n" (format_assign a2);
  let a1' = increment_value a1 3 in
  Printf.printf "increment_value a1 3 = %s\n\n" (format_assign a1');

  (* Part 3: StringMap *)
  let env = build_env [("x", 1); ("y", 2); ("z", 3)] in
  let print_lookup name =
    match lookup_var env name with
    | Some v -> Printf.printf "lookup_var \"%s\" = Some %d\n" name v
    | None   -> Printf.printf "lookup_var \"%s\" = None\n" name
  in
  print_lookup "x";
  print_lookup "y";
  print_lookup "w";
  Printf.printf "all_vars env = [%s]\n\n"
    (String.concat "; " (all_vars env));

  (* Part 4: StringSet *)
  let assignments = [a1; a2; a3] in
  let vars = assigned_vars assignments in
  Printf.printf "assigned_vars = {%s}\n"
    (String.concat ", " (StringSet.elements vars));
  let s1 = StringSet.of_list ["x"; "y"; "z"] in
  let s2 = StringSet.of_list ["y"; "z"; "w"] in
  let common = common_vars s1 s2 in
  Printf.printf "common_vars = {%s}\n\n"
    (String.concat ", " (StringSet.elements common));

  (* Part 5: ref *)
  let next = make_counter () in
  Printf.printf "counter: %d, %d, %d\n" (next ()) (next ()) (next ());

  Printf.printf "\nDone!\n"
