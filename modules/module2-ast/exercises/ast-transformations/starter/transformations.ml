  (* transformations.ml - AST transformation passes.

    Each transformation is a pure function: it takes an AST (or part of one)
    and returns a *new* AST with the transformation applied.  The original
    tree is never mutated.

    Implement the three transformations below.  Each one exercises a
    different kind of recursive tree rewriting. *)

    open Shared_ast.Ast_types

    (* --------------------------------------------------------------------------
      1. Constant folding
      --------------------------------------------------------------------------
      Simplify expressions whose operands are known at compile time.
    
      Strategy:
        - Recursively fold sub-expressions first (bottom-up).
        - After folding, check whether a BinOp has two IntLit children.
          If so, evaluate the operation and return a single IntLit (or BoolLit
          for comparison / logical operators).
        - Leave everything else unchanged.
    
      Examples:
        BinOp(Add, IntLit 2, IntLit 3)           --> IntLit 5
        BinOp(Mul, IntLit 2, BinOp(Add, IntLit 1, IntLit 3))
                                                    --> IntLit 8
        BinOp(Add, Var "x", IntLit 1)            --> BinOp(Add, Var "x", IntLit 1)
    *)
    let rec constant_fold (expr : expr) : expr =
      match expr with
      | IntLit _ | BoolLit _ | Var _ ->
          expr
      | Call (name, args) ->
          Call (name, List.map constant_fold args)
      | UnaryOp (op, e) ->
          let e' = constant_fold e in
          (match op, e' with
          | Neg, IntLit n -> IntLit (-n)
          | Not, BoolLit b -> BoolLit (not b)
          | _ -> UnaryOp (op, e'))
      | BinOp (op, e1, e2) ->
          let e1' = constant_fold e1 in
          let e2' = constant_fold e2 in
          match op, e1', e2' with
          | Add, IntLit a, IntLit b -> IntLit (a + b)
          | Sub, IntLit a, IntLit b -> IntLit (a - b)
          | Mul, IntLit a, IntLit b -> IntLit (a * b)
          | Div, IntLit a, IntLit b ->
              if b = 0 then BinOp (op, e1', e2') else IntLit (a / b)
          | Eq,  IntLit a, IntLit b -> BoolLit (a = b)
          | Neq, IntLit a, IntLit b -> BoolLit (a <> b)
          | Lt,  IntLit a, IntLit b -> BoolLit (a < b)
          | Gt,  IntLit a, IntLit b -> BoolLit (a > b)
          | Le,  IntLit a, IntLit b -> BoolLit (a <= b)
          | Ge,  IntLit a, IntLit b -> BoolLit (a >= b)
          | And, BoolLit a, BoolLit b -> BoolLit (a && b)
          | Or,  BoolLit a, BoolLit b -> BoolLit (a || b)
          | _ -> BinOp (op, e1', e2')
    
    (* --------------------------------------------------------------------------
      2. Variable renaming
      --------------------------------------------------------------------------
      Replace every occurrence of a variable named [old_name] with [new_name]
      throughout a list of statements.  This includes:
        - Var references inside expressions
        - The left-hand side of Assign statements
      Other identifiers (function names in Call, etc.) are left alone.
    
      You will need a helper to rename inside expressions as well.
    
      Example:
        rename_variable "x" "tmp"
          [Assign("x", IntLit 1); Print [Var "x"]]
        -->
          [Assign("tmp", IntLit 1); Print [Var "tmp"]]
    *)
    let rename_variable (old_name : string) (new_name : string)
        (stmts : stmt list) : stmt list =
      let rec rename_expr (e : expr) : expr =
        match e with
        | IntLit _ | BoolLit _ ->
            e
        | Var v ->
            if v = old_name then Var new_name else e
        | BinOp (op, e1, e2) ->
            BinOp (op, rename_expr e1, rename_expr e2)
        | UnaryOp (op, e1) ->
            UnaryOp (op, rename_expr e1)
        | Call (fname, args) ->
            Call (fname, List.map rename_expr args)
      in
      let rec rename_stmt (s : stmt) : stmt =
        match s with
        | Assign (name, e) ->
            let name' = if name = old_name then new_name else name in
            Assign (name', rename_expr e)
        | If (cond, then_branch, else_branch) ->
            If (rename_expr cond,
                List.map rename_stmt then_branch,
                List.map rename_stmt else_branch)
        | While (cond, body) ->
            While (rename_expr cond, List.map rename_stmt body)
        | Return None ->
            Return None
        | Return (Some e) ->
            Return (Some (rename_expr e))
        | Print exprs ->
            Print (List.map rename_expr exprs)
        | Block body ->
            Block (List.map rename_stmt body)
      in
      List.map rename_stmt stmts
    
    (* --------------------------------------------------------------------------
      3. Dead-code elimination
      --------------------------------------------------------------------------
      Remove statements that can never execute.  Two cases to handle:
    
      a) Unreachable code after Return:
          In a statement list, once a Return is encountered, all subsequent
          statements in that same list are dead and should be removed.
    
      b) Trivially-decided If:
          - If(BoolLit true,  then_branch, _)  --> replace with then_branch
          - If(BoolLit false, _, else_branch)   --> replace with else_branch
    
      Apply these rules recursively into nested blocks (If branches, While
      bodies, Block contents).
    
      Example:
        [Return (Some (IntLit 42)); Print [Var "unreachable"]]
        -->
        [Return (Some (IntLit 42))]
    *)
    let eliminate_dead_code (stmts : stmt list) : stmt list =
      let rec dce_stmt (s : stmt) : stmt list =
        match s with
        | Assign _ ->
            [s]
        | Return _ ->
            [s]
        | Print _ ->
            [s]
        | While (cond, body) ->
            [While (cond, dce_stmts body)]
        | Block body ->
            [Block (dce_stmts body)]
        | If (BoolLit true, then_branch, _) ->
            dce_stmts then_branch
        | If (BoolLit false, _, else_branch) ->
            dce_stmts else_branch
        | If (cond, then_branch, else_branch) ->
            [If (cond, dce_stmts then_branch, dce_stmts else_branch)]
    
      and dce_stmts (ss : stmt list) : stmt list =
        match ss with
        | [] -> []
        | s :: rest ->
            let s' = dce_stmt s in
            match s' with
            | [] ->
                dce_stmts rest
            | _ ->
                let has_return =
                  List.exists
                    (function Return _ -> true | _ -> false)
                    s'
                in
                if has_return then
                  s'
                else
                  s' @ dce_stmts rest
      in
      dce_stmts stmts
