(** Control Flow Graph implementation.

    Students: implement the functions marked with TODO below.
    [create_block] is provided as a reference. *)

    module StringSet = Set.Make(String)
    module StringMap = Map.Make(String)
    
    type basic_block = {
      label : string;
      stmts : Shared_ast.Ast_types.stmt list;
      mutable succs : string list;
      mutable preds : string list;
    }
    
    type cfg = {
      entry : string;
      exit_label : string;
      blocks : basic_block StringMap.t;
    }
    
    (* --- Provided ----------------------------------------------------------- *)
    
    let create_block (label : string) (stmts : Shared_ast.Ast_types.stmt list) : basic_block =
      { label; stmts; succs = []; preds = [] }
    
    (* --- TODO: implement these ---------------------------------------------- *)
    
    let add_edge (cfg : cfg) (src : string) (dst : string) : cfg =
      let src_block = StringMap.find src cfg.blocks in
      let dst_block = StringMap.find dst cfg.blocks in
    
      let updated_src = { src_block with succs = src_block.succs @ [dst] } in
      let updated_dst = { dst_block with preds = dst_block.preds @ [src] } in
    
      let updated_blocks =
        cfg.blocks
        |> StringMap.add src updated_src
        |> StringMap.add dst updated_dst
      in
      { cfg with blocks = updated_blocks }
    
    let predecessors (cfg : cfg) (label : string) : string list =
      let block = StringMap.find label cfg.blocks in
      block.preds
    
    let successors (cfg : cfg) (label : string) : string list =
      let block = StringMap.find label cfg.blocks in
      block.succs
    
    let to_string (cfg : cfg) : string =
      StringMap.fold
        (fun _ block acc ->
          let block_str =
            Printf.sprintf
              "Block: %s (%d stmts)\n  succs: [%s]\n  preds: [%s]\n"
              block.label
              (List.length block.stmts)
              (String.concat ", " block.succs)
              (String.concat ", " block.preds)
          in
          acc ^ block_str ^ "\n")
        cfg.blocks
        ""
