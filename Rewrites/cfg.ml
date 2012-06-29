open Language
open Language
open CFG

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
  | Case (x,_) -> (List.length x) + 1

let rec is_op = function
  | Opposite _ -> true
  | Brackets (x,_) -> is_op x
  | _ -> false

let rec make_stmt list = function
  | Block (x,_) as s -> 
    let enode =  make_block list in (* This is where I will continue to *)
    let n = make_block (List.rev x) in (* This is my own list *)
    let snode = Startnode (s,n) in
    let my_end_node = Endnode (s,enode,1) in
    get_new_block my_end_node snode
  | For (x,y,z,lc) as s -> 
    let node = make_block list in (* This is where I continue to *)
    let enode = Endnode (s,node,1) in
    (* Make sure that the vinit, vstride and vend are all expressions with correct Assign, etc*)
    let (vinit,vend,vstride,lcc) = (match y with | ColonExpr (x,y,z,l) -> (x,y,z,l) | _ -> failwith "Loop cannot have any, but colonExpr") in
    let vinitode = Squarenode (Assign ([AllTypedSymbol (SimTypedSymbol (DataTypes.Int32s, x,lc))],(SimExpr vinit),lc), 
			       (build_loop vend vstride lc enode x z)) in
    Startnode (s, vinitode)
  | Par (x,y,z,lc) as s ->
    let node = make_block list in (* This is where I continue to *)
    let enode = Endnode (s,node,1) in
    (* Make sure that the vinit, vstride and vend are all expressions with correct Assign, etc*)
    let (vinit,vend,vstride,lcc) = (match y with | ColonExpr (x,y,z,lc) -> (x,y,z,lc) | _ -> failwith "Loop cannot have any, but colonExpr") in
    let vinitode = Squarenode (Assign ([AllTypedSymbol (SimTypedSymbol (DataTypes.Int32s, x,lc))],(SimExpr vinit),lc), 
			       (build_loop vend vstride lc enode x z)) in
    Startnode (s, vinitode)
  | CaseDef (x,_) as s ->
    let node = make_block list in (* Where I need to continue to *)
    let enode = Endnode (s,node,(get_num_case x)) in
    let cnode = build_case x in
    let snode = Startnode (s, cnode) in
    get_new_block enode snode
  | _ as s -> Squarenode (s , make_block list) (* TODO: There shoud be no colon expr assigns left when coming here *)
and build_loop vend vstride lc enode x stmt = 
  let sexpr = Plus ((VarRef (x,lc)),Brackets(Cast(DataTypes.Int32s,vstride,lc),lc),lc) in (*FIXME: vstride needs to be of type Int32s*)
  (* First get the stmt list from the block stmt, iff it is a block stmt *)
  (* Needed, because in constant prpogation, the end-block is hit before
     the back-node, which removes all the stmts and then the stmts
     cannot be replced with top *)
  let stmt_list = (match stmt with Block (x,_) -> x | _ -> []) in
  let tbranch = 
    if ( List.length stmt_list > 1) then 
      let to_send = List.tl (List.rev stmt_list) in
      let stmt = List.hd (List.rev stmt_list) in
      make_stmt (to_send@[(Assign ([AllSymbol x],(SimExpr sexpr),lc))]) stmt
    else if (List.length stmt_list = 1) then (make_stmt [(Assign ([AllSymbol x],(SimExpr sexpr),lc))] (List.hd stmt_list))
    (* Note that the list gets attached at the end of the tree *)
    else make_stmt [Assign ([AllSymbol x],(SimExpr sexpr),lc)] stmt
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
and make_block = function
  | h::t -> make_stmt t h;
  | [] -> Empty
and build_case = function
  | Case (x,y) -> 
    let ostmt = make_stmt [] (match y with Otherwise x -> x) in
    build_case_clause ostmt (List.rev x)
and build_case_clause ostmt = function
  | h::t -> build_clause_stmt t ostmt h
  | [] -> ostmt
and build_clause_stmt t ostmt = function
      (* The Empty needs to get replaced with else if or otherwise stmts *)
  | Clause (x,y) -> Conditionalnode (x, (make_stmt [] y), (build_case_clause ostmt t) )


let rec make_cfg r e = function
  | Filter (x,y,z,w) -> 
    let (name,lc) = (match x with Symbol (x,lc) -> (x,lc)) in
    let inl = get_it lc y in
    let outl = get_it lc z in
    let node = make_stmt [] w in
    Topnode (e, name, r, [inl;outl;node])
and get_it lc = function
  | h :: t -> Squarenode (VarDecl (h,lc), get_it lc t)
  | [] -> Empty

let rec check_fcfg = function
  | FCFG.Node (e,x,r,y) -> 
    (* Me is a top node *)
    let me = make_cfg r e x in 
    (* These are all also top nodes for all the other filters *)
    let ll = check_fcfg_nodes y in
    Filternode (me, ll)
and check_fcfg_nodes = function
  | h::t -> check_fcfg h::check_fcfg_nodes t
  | [] -> [] (* These are the topnode nodes list *)
