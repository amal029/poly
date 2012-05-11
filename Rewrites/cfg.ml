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
  | Endnode (y,x) -> Endnode (y, (get_new_block enode x))
  | Empty -> enode
  | Backnode _ as s -> s

let rec make_stmt list = function
  | Block x as s -> 
    let enode =  make_block list in (* This is where I will continue to *)
    let n = make_block x in (* This is my own list *)
    let snode = Startnode (s,n) in
    let my_end_node = Endnode (s,enode) in
    get_new_block snode my_end_node
  | For (x,y,z) | Par (x,y,z) as s ->
    let node = make_block list in (* This is where I continue to *)
    let enode = Endnode (s,node) in
    (* Make sure that the vinit, vstride and vend are all expressions with correct Assign, etc*)
    let (vinit,vend,vstride) = (match y with | ColonExpr (x,y,z) -> (x,y,z) | _ -> failwith "Loop cannot hvave any, but colonExpr") in
    let vinitode = Squarenode (Assign ([AllTypedSymbol (SimTypedSymbol (DataTypes.Int32, x))],(SimExpr vinit)), 
			       (build_loop vend vstride enode x z)) in
    Startnode (s, vinitode)
  | CaseDef x as s ->
    let node = make_block list in (* Where I need to continue to *)
    let enode = Endnode (s,node) in
    let cnode = build_case x in
    let snode = Startnode (s, cnode) in
    get_new_block snode enode
  | _ as s -> Squarenode (s , make_block list) (* TODO: There shoud be no colon expr assigns left when coming here *)
and build_loop vend vstride enode x stmt = 
  let sexpr = Plus ((VarRef x),vstride) in
  let cond = Conditionalnode (LessThanEqual ((VarRef x), vend), make_stmt [Assign ([AllSymbol x],(SimExpr sexpr))] stmt (* true child *),  enode (* false child *)) in
  let bedge = ref (Backnode (ref Empty)) in
  (* replace the Empty in the true branch with the back edge *)
  let rcond = get_new_block !bedge cond in
  bedge := Backnode (ref rcond);
  rcond
and make_block = function
  | h::t -> make_stmt t h;
  | [] -> Empty
and build_case = function
  | Case (x,y) -> 
    let ostmt = make_stmt [] (match y with Otherwise x -> x) in
    build_case_clause ostmt x
and build_case_clause ostmt = function
  | h::t -> build_clause_stmt t ostmt h
  | [] -> ostmt
and build_clause_stmt t ostmt = function
      (* The Empty needs to get replaced with else if or otherwise stmts *)
  | Clause (x,y) -> Conditionalnode (x, (make_stmt [] y), (build_case_clause ostmt t) )


let rec make_cfg e = function
  | Filter (x,y,z,w) -> 
    let name = (match x with Symbol x -> x) in
    let inl = get_it y in
    let outl = get_it z in
    let node = make_stmt [] w in
    Topnode (e, name, [inl;outl;node])
and get_it = function
  | h :: t -> Squarenode (VarDecl h, get_it t)
  | [] -> Empty

let rec check_fcfg = function
  | FCFG.Node (e,x,y) -> 
    (* Me is a top node *)
    let me = make_cfg e x in 
    (* These are all also top nodes for all the other filters *)
    let ll = check_fcfg_nodes y in
    Filternode (me, ll)
and check_fcfg_nodes = function
  | h::t -> check_fcfg h::check_fcfg_nodes t
  | [] -> [] (* These are the topnode nodes list *)
