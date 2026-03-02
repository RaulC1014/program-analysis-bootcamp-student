(* ================================================================
   Exercise 4: Modules and Functors -- "Analysis Domain Builder"
   ================================================================

   Theme: You will build a lattice module system that directly
   mirrors lib/abstract_domains/ from Modules 3-4. By the time
   you reach those modules, the pattern will be familiar.

   Concepts: module types (signatures), module structs, functors,
   Map.Make, opening modules.

   Run with:  dune exec modules/module0-warmup/exercises/modules-and-functors/starter/main.exe
   ================================================================ *)

(* ----------------------------------------------------------------
   Part 1: Module Types (Signatures)

   A LATTICE is the fundamental interface for abstract domains.
   Every abstract domain in the bootcamp satisfies this signature.
   ---------------------------------------------------------------- *)

(** The LATTICE signature: an abstract domain with bottom, top,
    join, equality test, and string conversion. *)
module type LATTICE = sig
  type t
  val bottom : t
  val top : t
  val join : t -> t -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end

(* ----------------------------------------------------------------
   Part 2: A Simple Lattice -- BoolLattice

   The simplest non-trivial lattice: { false, true } where
   false is bottom, true is top, and join is logical OR.

   This is COMPLETE -- study it as a model for Part 3.
   ---------------------------------------------------------------- *)

module BoolLattice : LATTICE with type t = bool = struct
  type t = bool
  let bottom = false
  let top = true
  let join a b = a || b
  let equal a b = (a = b)
  let to_string b = if b then "true" else "false"
end

(** Helper to print BoolLattice values. *)
module BoolPrint = struct
  (** [to_string b] converts a bool to "T" or "F". *)
  let to_string (b : bool) : string =
    if b then "T" else "F"
  end

(* ----------------------------------------------------------------
   Part 3: A Three-Value Lattice

   This models the "sign" idea from Module 4: we track whether
   a value is Zero, Positive, or Unknown (top).

       Unknown (top)
       /      \
    Zero    Positive
       \      /
        Bot (bottom)

   We define the type at the top level so both the module
   implementation and the test code can refer to the constructors.
   ---------------------------------------------------------------- *)

(** The three-value lattice type, defined at top level for visibility. *)
type three_value = Bot | Zero | Positive | Unknown

module ThreeValueLattice : LATTICE with type t = three_value = struct
  type t = three_value

  (* These are provided -- they are trivial one-liners. *)
  let bottom = Bot
  let top = Unknown

  (** [join a b] computes the least upper bound:
      - join x x = x  (same values)
      - join Bot x = x and join x Bot = x  (Bot is identity)
      - join _ _ = Unknown  (all other cases) *)
  let join (a : t) (b : t) : t =
    match a, b with
    |x, y when x = y -> x
    |Bot, x -> x
    |x, Bot -> x
    |_ -> Unknown

  (** [equal a b] returns true if a and b are the same variant. *)
  let equal (a : t) (b : t) : bool =
    a = b

  (** [to_string v] returns "Bot", "Zero", "Positive", or "Unknown". *)
  let to_string (v : t) : string =
    match v with
    | Bot -> "Bot"
    | Zero -> "Zero"
    | Positive -> "Positive"
    | Unknown -> "Unknown"
end

(* ----------------------------------------------------------------
   Part 4: A Functor -- MakeEnv

   In Modules 3-4, abstract environments map variable names to
   abstract values. The MakeEnv functor takes any LATTICE and
   produces an environment module with lookup, update, and join.

   This directly mirrors lib/abstract_domains/abstract_env.ml.
   ---------------------------------------------------------------- *)

module MakeEnv (L : LATTICE) = struct
  module M = Map.Make(String)

  (** An environment maps variable names to lattice values. *)
  type t = L.t M.t

  (** The empty environment. *)
  let empty : t = M.empty

  (** [lookup env x] returns the lattice value for variable [x],
      or [L.bottom] if [x] is not in the environment. *)
  let lookup (env : t) (x : string) : L.t =
    match M.find_opt x env with
    | None -> L.bottom
    | Some v -> v

  (** [update env x v] returns a new environment with [x] mapped
      to [v]. *)
  let update (env : t) (x : string) (v : L.t) : t =
    M.add x v env

  (** [join env1 env2] merges two environments by joining values
      for each variable that appears in either.

      Hint: use M.union which takes a function
        (key -> v1 -> v2 -> Some merged_value) *)
  let join (env1 : t) (env2 : t) : t =
    M.union (fun _ v1 v2 -> Some (L.join v1 v2)) env1 env2

  (** [to_string env] returns a string like "{x -> Zero, y -> Positive}". *)
  let to_string (env : t) : string =
    let pairs = M.bindings env in
    let entries =
      List.map (fun (k, v) -> k ^ " -> " ^ L.to_string v) pairs
    in
    "{" ^ String.concat ", " entries ^ "}"
end

(* ================================================================
   Main -- instantiate the functor and test everything.
   ================================================================ *)

(** Instantiate MakeEnv with ThreeValueLattice. *)
module Env = MakeEnv(ThreeValueLattice)

let () =
  Printf.printf "=== Exercise 4: Modules and Functors ===\n\n";

  (* Part 2: BoolLattice *)
  Printf.printf "-- BoolLattice --\n";
  Printf.printf "BoolPrint.to_string true = %s\n" (BoolPrint.to_string true);
  Printf.printf "BoolPrint.to_string false = %s\n" (BoolPrint.to_string false);
  Printf.printf "BoolLattice.join false true = %s\n"
    (BoolLattice.to_string (BoolLattice.join false true));
  Printf.printf "BoolLattice.equal true true = %b\n\n"
    (BoolLattice.equal true true);

  (* Part 3: ThreeValueLattice *)
  Printf.printf "-- ThreeValueLattice --\n";
  Printf.printf "bottom = %s\n"
    (ThreeValueLattice.to_string ThreeValueLattice.bottom);
  Printf.printf "top = %s\n"
    (ThreeValueLattice.to_string ThreeValueLattice.top);
  Printf.printf "join Bot Zero = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Bot Zero));
  Printf.printf "join Zero Positive = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Zero Positive));
  Printf.printf "join Positive Positive = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Positive Positive));
  Printf.printf "equal Zero Zero = %b\n"
    (ThreeValueLattice.equal Zero Zero);
  Printf.printf "equal Zero Positive = %b\n\n"
    (ThreeValueLattice.equal Zero Positive);

  (* Part 4: MakeEnv *)
  Printf.printf "-- MakeEnv(ThreeValueLattice) --\n";
  let env0 = Env.empty in
  Printf.printf "empty = %s\n" (Env.to_string env0);
  Printf.printf "lookup empty \"x\" = %s\n"
    (ThreeValueLattice.to_string (Env.lookup env0 "x"));

  (* Build environments and join them *)
  let env1 = Env.update env0 "x" Zero in
  let env1 = Env.update env1 "y" Positive in
  Printf.printf "env1 = %s\n" (Env.to_string env1);

  let env2 = Env.update Env.empty "x" Positive in
  let env2 = Env.update env2 "z" Zero in
  Printf.printf "env2 = %s\n" (Env.to_string env2);

  let merged = Env.join env1 env2 in
  Printf.printf "join env1 env2 = %s\n" (Env.to_string merged);

  Printf.printf "\nDone!\n"
