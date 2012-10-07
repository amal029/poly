open Language
open Language
open CFG

exception Internal_compiler_error of string

let my_vectorize = ref false

let rec get_new_block enode = function
  | Startnode (x,y) -> Startnode (x,get_new_block enode  y)
  | Squarenode (x,y) -> Squarenode (x,get_new_block enode  y)
  | Conditionalnode (z,x,y) -> 
    let tb = get_new_block enode x in
    let fb = get_new_block enode y in
    Conditionalnode (z,tb,fb)
  | Endnode (y,x,z) -> Endnode (y, (get_new_block enode x), z)
  | Empty -> enode
  | Backnode _ as s -> s

let get_num_case = function
  | Case (x,_,_) -> (List.length x) + 1

let rec is_op = function
  | Opposite _ -> true
  | Brackets (x,_) -> is_op x
  | _ -> false

let rec make_stmt symbol_table list = function
  | Block (x,_) as s -> 
    let enode =  make_block symbol_table list in (* This is where I will continue to *)
    let n = make_block symbol_table x in (* This is my own list *)
    let snode = Startnode (s,n) in
    let my_end_node = Endnode (s,enode,1) in
    get_new_block my_end_node snode

  | Split (x,y) as s -> 
    let x = (match x with Block (x,_) -> x) in
    let enode =  make_block symbol_table list in (* This is where I will continue to *)
    let n = make_block symbol_table x in (* This is my own list *)
    let snode = Startnode (s,n) in
    let my_end_node = Endnode (s,enode,1) in
    get_new_block my_end_node snode
      
  | For (x,y,z,lc) as s -> 
    (* Add the loop induction variable to the symbol_table *)
    let rsym = symbol_table in
    symbol_table := (SimTypedSymbol (DataTypes.Int32s, x, lc)):: !symbol_table; 
    let node = make_block symbol_table list in (* This is where I continue to *)
    let enode = Endnode (s,node,1) in
    (* Make sure that the vinit, vstride and vend are all expressions with correct Assign, etc*)
    let (vinit,vend,vstride,lcc) = (match y with | ColonExpr (x,y,z,l) -> (x,y,z,l) | _ -> failwith "Loop cannot have any, but colonExpr") in
    let vinitode = Squarenode (Assign ([AllTypedSymbol (SimTypedSymbol (DataTypes.Int32s, x,lc))],(SimExpr vinit),lc), 
			       (build_loop symbol_table vend vstride lc enode x z)) in
    symbol_table := !rsym;
    Startnode (s, vinitode)

  | Par (x,y,z,lc) as s ->
    (* Par can be converted to a vector intruction possibly!! *)
    (*

      Conditions under which par will be converted to vector type:
      
      1.) No vardeclarations inside the par statement

      2.) No function calls within the par stmt
      
      3.) proper access to indices and sizes
      
      4.) If there is a function call then we will enter the function
      call and parallelize that part!! Note, do not write scalar
      functions at all, if you want vectorization!!
      
    *)
    (try 
       if (!my_vectorize) && (Vectorization.SafeToConvert.process_par s) then
	 (* We will vectorize the damn thing completely!! *)
	 (* This thing will always give a block back!! *)
	 (* 
	    First we need to calculate the iteration space for this thing! 
	    
	    Then 

	    1.) If the next statement is a par statement then we will
	    call this same function but with setting the conversion to
	    COLLAPSE constructor and sending the iteration vector space
	    of myself.
	    
	    We will check the safety conditions for collapse before
	    calling collapse.

	    --> Safe conditions that might vectorize the par loop
            statements inside.

	    a.) R[i][j] = R[i][j] + 1 i.e., all the indices need to be
	    separate and should only be associated with constants (or
	    later on invariants).

	    2.) If the next statement is a simple statement then I will
	    just call convert as it is now.

	    
	    3.) If the next statement is a for or it cannot be collapsed
	    then I will Give an error. --> I will then call the loop
	    interchange function which will send a new loop statement
	    (just for this loop) with the loops interchanged I will then
	    call normal
	    

	 *)
	 let enode =  make_block symbol_table list in (* This is where I will continue to *)
	 (* Check if your child is a par stmt, if it is then call yourself!! *)
	 let (vec_block as s ) = LoopCollapse.convert !symbol_table s in
	 let x = (match vec_block with | Block (x,_) -> x | _ -> raise (Internal_compiler_error((Reporting.get_line_and_column lc) ^ " not of Block type"))) in
	 let n = make_block symbol_table x in (* This is my own list *)
	 let snode = Startnode (s,n) in
	 let my_end_node = Endnode (s,enode,1) in
	 get_new_block my_end_node snode 
       else raise (Vectorization.Convert.Error "Not the lowest par loop or one with var decs, etc")
     with
       | Vectorization.Convert.Error e | Vectorization.Convert.Internal_compiler_error e -> 
	 let () = print_endline ("Warning: not converting par statement " ^ (Dot.dot_stmt s) ^ " to a vector") in 
	 let () = print_endline ("Because of :" ^ e) in
	 let rsym = symbol_table in
	 symbol_table := (SimTypedSymbol (DataTypes.Int32s, x, lc)):: !symbol_table; 
	 let node = make_block symbol_table list in (* This is where I continue to *)
	 let enode = Endnode (s,node,1) in
	 (* Make sure that the vinit, vstride and vend are all expressions with correct Assign, etc*)
	 let (vinit,vend,vstride,lcc) = (match y with | ColonExpr (x,y,z,lc) -> (x,y,z,lc) | _ -> failwith "Loop cannot have any, but colonExpr") in
	 let vinitode = Squarenode (Assign ([AllTypedSymbol (SimTypedSymbol (DataTypes.Int32s, x,lc))],(SimExpr vinit),lc), 
				    (build_loop symbol_table vend vstride lc enode x z)) in
	 symbol_table := !rsym;
	 Startnode (s, vinitode))

  | CaseDef (x,_) as s ->
    let node = make_block symbol_table list in (* Where I need to continue to *)
    let enode = Endnode (s,node,(get_num_case x)) in
    let cnode = build_case symbol_table x in
    let snode = Startnode (s, cnode) in
    get_new_block enode snode

  | _ as s -> Squarenode (s , make_block symbol_table list) (* TODO: There shoud be no colon expr assigns left when coming here *)

and build_loop symbol_table vend vstride lc enode x stmt = 
  let sexpr = Plus ((VarRef (x,lc)),Brackets(Cast(DataTypes.Int32s,vstride,lc),lc),lc) in (*FIXME: vstride needs to be of type Int32s*)
  (* First get the stmt list from the block stmt, iff it is a block stmt *)
  (* Needed, because in constant prpogation, the end-block is hit before
     the back-node, which removes all the stmts and then the stmts
     cannot be replced with top *)
  let stmt_list = (match stmt with Block (x,_) -> x | _ -> []) in
  let tbranch = 
    if ( List.length stmt_list > 1) then 
      let to_send = List.tl stmt_list in
      let stmt = List.hd stmt_list in
      make_stmt symbol_table (to_send@[(Assign ([AllSymbol x],(SimExpr sexpr),lc))]) stmt
    else if (List.length stmt_list = 1) then (make_stmt symbol_table [(Assign ([AllSymbol x],(SimExpr sexpr),lc))] (List.hd stmt_list))
    (* Note that the list gets attached at the end of the tree *)
    else make_stmt symbol_table [Assign ([AllSymbol x],(SimExpr sexpr),lc)] stmt
  in
  (* let tbranch = make_stmt [Assign ([AllSymbol x],(SimExpr sexpr))] stmt in *)
  let back = ref Empty in
  let bedge = (Backnode back) in
  (* replace the Empty in the true branch with the back edge *)
  let ntbranch = get_new_block bedge tbranch in
  (* Get the polarity of this loop *)
  let isoop = is_op vstride in
  let cond = 
    (if (not isoop) then Conditionalnode (LessThanEqual ((VarRef (x,lc)), vend,lc),  ntbranch (* true child *),  enode (* false child *))
     else Conditionalnode (GreaterThanEqual ((VarRef (x,lc)), vend,lc),  ntbranch (* true child *),  enode (* false child *))
    ) in
  back := cond;
  (*Debugging*)
  (* print_endline (match bedge with Backnode x -> if (!x == cond) then "true" else "false"); *)
  cond
and make_block symbol_table = function
  | h::t -> make_stmt symbol_table t h;
  | [] -> Empty
and build_case symbol_table = function
  | Case (x,y,_) -> 
    let ostmt = make_stmt symbol_table [] (match y with Otherwise (x,_) -> x) in
    build_case_clause symbol_table ostmt x
and build_case_clause symbol_table ostmt = function
  | h::t -> build_clause_stmt symbol_table t ostmt h
  | [] -> ostmt
and build_clause_stmt symbol_table t ostmt = function
  (* The Empty needs to get replaced with else if or otherwise stmts *)
  | Clause (x,y,_) -> Conditionalnode (x, (make_stmt symbol_table [] y), (build_case_clause symbol_table ostmt t) )


let rec make_cfg r e = function
  | Filter (x,y,z,w) -> 
    let (name,lc) = (match x with Symbol (x,lc) -> (x,lc)) in
    let inl = get_it lc y in
    let outl = get_it lc z in
    let symbol_table = ref [] in
    let () = Vectorization.Convert.build_symbol_table symbol_table w in
    symbol_table := (y@z) @ !symbol_table;
    let node = make_stmt symbol_table [] w in
    Topnode (e, name, r, [inl;outl;node])
and get_it lc = function
  | h :: t -> Squarenode (VarDecl (h,lc), get_it lc t)
  | [] -> Empty

let rec check_fcfg vectorize = function
  | FCFG.Node (e,x,r,y) ->
    my_vectorize := vectorize;
    (* Me is a top node *)
    let me = make_cfg r e x in 
    (* These are all also top nodes for all the other filters *)
    let ll = check_fcfg_nodes vectorize y in
    Filternode (me, ll)
and check_fcfg_nodes vectorize = function
  | h::t -> check_fcfg vectorize h::check_fcfg_nodes vectorize t
  | [] -> [] (* These are the topnode nodes list *)
