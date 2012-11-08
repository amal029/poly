open Language
open Language
open CFG
open Odot

exception Error of string
exception Internal_compiler_error of string

let counter = ref 0
let visited_nodes = Hashtbl.create 50
(* let cond_nodes = Hashtbl.create 10 *)

let get_symbol = function
  | Symbol (x,_) -> x

let get_addressed_symbol = function
  | AddressedSymbol (x,_,_,_) -> get_symbol x

let get_typed_symbol = function
  | SimTypedSymbol (_,x,(l,c)) -> get_symbol x ^ ":" ^ (string_of_int l) ^ "," ^ (string_of_int c)
  | ComTypedSymbol (_,x,(l,c)) -> get_addressed_symbol x ^ ":" ^ (string_of_int l) ^ "," ^ (string_of_int c)
    
let rec dot_simpleexpr = function
  | TStar -> "*"
  | TStarStar -> "**"
  | Mod (x,y,_) -> (dot_simpleexpr x ^ "%" ^ dot_simpleexpr y)
  | Plus (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " + " ) ^ rstring
  | Minus (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " - " ) ^ rstring
  | Times (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " x " ) ^ rstring
  | Abs (x,_)  -> 
    let lstring = dot_simpleexpr x in
    ("|" ^ lstring ^ "|" )
  | Div (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " / " ) ^ rstring
  | Pow (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " ^ " ) ^ rstring
  | Lshift (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " << " ) ^ rstring
  | Rshift (x,y,_)  -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " >> " ) ^ rstring
  | Const (x,y,_) -> ((DataTypes.print_datatype x) ^ " ") ^ y
  | VarRef (x,_) -> get_symbol x
  | AddrRef (x,_) -> get_addressed_string x
  | ColonExpr (x,y,z,_) -> ((dot_simpleexpr x) ^ " : " ^ (dot_simpleexpr y)) ^ " : " ^ (dot_simpleexpr z)
  | Opposite (x,_) -> "-" ^ dot_simpleexpr x
  | Brackets (x,_) -> dot_simpleexpr x
  | Cast (x,y,_) -> (("(" ^ DataTypes.print_datatype x) ^ ")") ^ (dot_simpleexpr y)
  | VecRef (ar,x,_) -> 
    let sm = List.fold_right (fun x y -> (dot_simpleexpr x) ^ "," ^ y) ar " " in
    sm ^ ("<" ^ get_vec_string x ^ ">")
  | Constvector (_,d,x,_) ->(("(" ^ DataTypes.print_datatype d) ^ ")") ^ "< " ^ (Array.fold_right (fun x y -> dot_simpleexpr x ^ "," ^ y) x "") ^ " >"
  | Vector (d,x,l,_) ->"<" ^ (DataTypes.print_datatype d) ^ " " ^ (dot_simpleexpr x) ^ "(" ^ (string_of_int l) ^ ")>" 

and get_addressed_string = function
  | AddressedSymbol (x,_,z,_) -> 
    let name = get_symbol x in
    let string_brac_dims = get_string_brac_dims z in
    name ^ string_brac_dims

and get_vec_string = function
  | VecAddress (x,_,y,_) -> (get_symbol x) ^ "[" ^ (List.fold_right (fun x y -> dot_simpleexpr x) y " ") ^ "]"

and get_string_brac_dims = function
  | h::t -> (("[" ^ (get_bracdim h) ) ^ "]") ^ (get_string_brac_dims t)
  | [] -> ""
and get_bracdim = function
  | BracDim x -> get_string_dimspec_list x
and get_string_dimspec_list = function
  | h::t -> ((get_string_dimspec h) ^ (if (List.length t <> 0) then "," else "")) ^ (get_string_dimspec_list t)
  | [] -> ""
and get_string_dimspec = function
  | DimSpecExpr x -> (dot_simpleexpr x)

let dot_typed_symbol = function
  | SimTypedSymbol (x,y,_) -> ((DataTypes.print_datatype x) ^ " ") ^ (get_symbol y)
  | ComTypedSymbol (x,y,_) -> ((DataTypes.print_datatype x) ^ " ") ^ (get_addressed_string y)


let dot_allsym = function
  | AllTypedSymbol x -> (dot_typed_symbol x)
  | AllSymbol x -> (get_symbol x)
  | AllAddressedSymbol x -> (get_addressed_string x)
  | AllVecSymbol (ar,x) -> 
    let sm = Array.fold_right (fun x y -> (string_of_int x) ^ y) ar.ism " " in
    let msm = List.fold_right (fun x y -> (dot_simpleexpr x) ^ "," ^ y) ar.sm " " in
    sm ^ get_vec_string x

let rec dot_allsym_list = function
  | h::t -> ((dot_allsym h) ^ if (List.length t <> 0) then "," else " ") ^ (dot_allsym_list t)
  | [] -> ""

let dot_callargument = function
  | CallAddrressedArgument x -> (get_addressed_string x)
  | CallSymbolArgument x -> get_symbol x

let rec dot_callargument_list = function
  | h::t -> ((dot_callargument h) ^ (if (List.length t <> 0) then  "," else "")) ^ (dot_callargument_list t)
  | [] -> ""

let dot_filtercall = function
  | Call (x,y,_) -> (get_symbol x) ^ (( "(" ^ (dot_callargument_list y)) ^ ")" )

let dot_expr = function
  | SimExpr x -> (dot_simpleexpr x)
  (* | Main -> "Main" *)
  | FCall (x,extern) -> let t = dot_filtercall x in if extern then ("extern" ^ t) else t

let rec dot_stmt = function
  | Assign (x,y,(l,c),spe) -> 
    let rvalue = (dot_expr y) in
    let spe = (match spe with Some x -> (match x with (NVVM _) -> "NVVM" | _ -> raise (Internal_compiler_error "")) | None -> "") in
    let lvalue = (( "(" ^ (dot_allsym_list x)) ^ " " ^ spe ^ " )") in
    (* FIXME: I am attaching this to the required nodes for graph-part
       to work correctly, with tiling, but this should be moved into its
       own function *)
    (lvalue ^ " = " ) ^ rvalue ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)
  | VarDecl (x,_) -> (dot_typed_symbol x)
  | Block (x,(l,c)) -> 
    let ll = List.map (fun x -> dot_stmt x) x in
    let tot = ref "" in
    let () = (List.iter (fun x -> tot := ((!tot ^ x)) ) ll ) in
    "{" ^  !tot ^ "}" ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)
  | Escape (x,(l,c)) -> x ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)
  | Noop -> "Noop"
  | For (x,y,z,(l,c)) -> 
    let e1 = get_symbol x in
    let e2 = dot_simpleexpr y in
    let e3 = dot_stmt z in
    (" For ") ^ (e1 ^ " " ) ^ (e2 ^ " ") ^ (e3) ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)
  | Par (x,y,z,(l,c)) -> 
    let e1 = get_symbol x in
    let e2 = dot_simpleexpr y in
    let e3 = dot_stmt z in
    " Par " ^ (e1 ^ " " ) ^ (e2 ^ " ") ^ (e3) ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)
  | CaseDef (x,(l,c)) -> "CaseDef" ^ dot_case x ^ (string_of_int l) ^ (string_of_int c)
  | Split (x,(l,c)) -> "Split " ^ dot_stmt x ^ ":" ^ (string_of_int l) ^"," ^ (string_of_int c)

and dot_case = function
  | Case (x,o,_) -> List.fold_right (fun x y -> dot_caseclause x ^ y) x (dot_otherwise o)
and dot_otherwise = function
  | Otherwise (x,_) -> "Otherwise " ^ dot_stmt x
and dot_caseclause = function
  | Clause (r,x,_) -> (dot_relexpr r) ^ dot_stmt x
and dot_relexpr = function
  | LessThan (x,y,_) -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " < " ) ^ rstring
  | GreaterThan (x,y,_) -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " > " ) ^ rstring
  | LessThanEqual (x,y,_) -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " <= " ) ^ rstring
  | GreaterThanEqual (x,y,_) -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " >= " ) ^ rstring
  | EqualTo (x,y,_) -> 
    let lstring = dot_simpleexpr x in
    let rstring = dot_simpleexpr y in
    (lstring ^ " == " ) ^ rstring
  | And (lx, rx, _) -> 
    let ll = dot_relexpr lx in
    let lr = dot_relexpr rx in
    (ll ^ "&&" ^ lr)
  | Or (lx, rx, _) -> ((dot_relexpr lx) ^ "||" ^ (dot_relexpr rx))
  | Rackets (x,_) -> "(" ^ (dot_relexpr x) ^ ")"

let get_tnode_id = function
  | Stmt_edge x -> (match x with (x,_,_) -> (match x with Edge_node_id x -> x | _ -> raise (Internal_compiler_error "Got an edge_subgraph when not expected")))
  | Stmt_node (x,_) -> x
  | _ -> raise (Internal_compiler_error "Got something other than stmt_edge/stmt_node, while building the dot graph")

let rec dot_cfg ll = function
  | Startnode (_,y) ->
    (* let () = print_endline "Got a start node" in *)
    let cfg_node = dot_cfg ll y in
    counter := !counter + 1;
    let this_id = (Simple_id ("Startnode" ^ (string_of_int !counter)), None) in
    let this = Stmt_node (this_id , [(Simple_id("shape"), Some (Simple_id("circle")))]) in 
    let tnode = (get_tnode_id cfg_node) in
    let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id tnode], []) in
    ll := this :: !ll;
    ll := ret :: !ll;
    ret

  | Squarenode (x,y) -> 
    (* let () = print_endline "Got a square node" in *)
    let cfg_node = dot_cfg ll y in
    counter := !counter + 1;
    let this_id = (Simple_id ("Squarenode" ^ (string_of_int !counter)), None) in
    let this_label = ("\""^ (dot_stmt x)) ^ "\"" in
    let this = Stmt_node (this_id, [(Simple_id("shape"),Some(Simple_id("box")));
				    (Simple_id("label"),Some(Simple_id(this_label)))]) in
    let tnode = get_tnode_id cfg_node in
    let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id tnode], []) in
    ll := this :: !ll;
    ll := ret :: !ll;
    ret

  | Conditionalnode (expr, t, f) as s ->
    (try
       Hashtbl.find visited_nodes s
     with
       | Not_found ->
	 counter := !counter + 1;
	 let this_id = (Simple_id("Conditionalnode" ^ (string_of_int !counter)), None) in
	 let tbranch = dot_cfg ll t in
	 let fbranch = dot_cfg ll f in
	 let this_label = ("\"" ^ (dot_relexpr expr)) ^ "\"" in
	 let this = Stmt_node (this_id, [(Simple_id("shape"),Some (Simple_id("diamond")));
					 (Simple_id("label"),Some(Simple_id(this_label)))]) in
	 let tnode1 = get_tnode_id tbranch in
	 let tnode2 = get_tnode_id fbranch in
	 let ret = Stmt_edge ((Edge_node_id this_id),  [(Edge_node_id tnode1)], [(Simple_id("label"),Some (Simple_id("True")))]) in
	 let ret2 = Stmt_edge ((Edge_node_id this_id),  [(Edge_node_id tnode2)], [(Simple_id("label"),Some (Simple_id("False")))]) in
	 ll := this :: !ll;
	 ll := ret :: !ll;
	 ll := ret2 :: !ll;
	 ret2)
	  
  | Endnode (x,y,v) as s -> 
    (try
       Hashtbl.find visited_nodes s
     with
       | Not_found ->
	 (* let () = print_endline "Got an end node" in *)
	 let cfg_node = dot_cfg ll y in
	 counter := !counter + 1;
	 let this_id = (Simple_id("Endnode" ^ (string_of_int !counter)), None) in
	 let this = Stmt_node (this_id , [(Simple_id("shape"),Some(Simple_id("circle")))]) in 
	 let tnode = get_tnode_id cfg_node in
	 let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id tnode], []) in
	 ll := this :: !ll;
	 ll := ret :: !ll;
	(* Insert in the hashtbl *)
	 Hashtbl.add visited_nodes s ret;
	 ret)

  | Backnode reff as s ->
    (try
       Hashtbl.find visited_nodes s
     with
       | Not_found ->
	 (* let () = print_endline "Got a back node" in *)
	 counter := !counter + 1;
	 let this_id = (Simple_id("Backnode" ^ (string_of_int !counter)), None) in
	 (* Get the name of the real conditional node*)
	 let expr_string = (
	   match !reff with
	     | Conditionalnode (x,_,_) -> dot_relexpr x
	     | _ -> raise (Internal_compiler_error "Backnode not attached to a conditional node")
	 ) in
	 let expr_string = "backNode connected to -- " ^ expr_string in
	 (* let () = print_endline expr_string in *)
	 let ret = Stmt_node (this_id , [(Simple_id("shape"),Some(Simple_id("pentagon")));
					 (Simple_id("label"),Some(Simple_id((("\"" ^ expr_string) ^ "\""))))]) in 
	 (* let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id ref_id], [(Simple_id("style"), Some(Simple_id("dotted")))]) in *)
	 ll := ret :: !ll;
	 Hashtbl.add visited_nodes s ret;
	 ret)
      

  | Empty as s -> 
    (try
       Hashtbl.find visited_nodes s
    with
      | Not_found ->
	(* let () = print_endline "Got an empty node" in *)
	counter := !counter + 1;
	let this_id = (Simple_id("Empty" ^ (string_of_int !counter)), None) in
	let ret = Stmt_node (this_id, []) in
	Hashtbl.add visited_nodes s ret;
	ret
    )

(* For debugging I need to dump all top-nodes separately!!  thigs are
   too big for dot and for me *)

let fcounter = ref 1

let dot_topnode = function
  | Topnode (fcall, name, r, cfg_list,_) -> 
    (* Debugging *)
    (* let () = print_endline ("Building new filter named************: " ^ name) in *)
    let cfg_dot_list = ref [] in 
    let _ = List.map (dot_cfg cfg_dot_list) cfg_list in
    let id = Simple_id name in
    (*Clear the visited_nodes Hashtbl *)
    Hashtbl.clear visited_nodes;
    (* Dump the files separately *)
    fcounter := !fcounter + 1;
    let rdot = (match r with | None -> "" | Some x -> ("where" ^ dot_relexpr x)) in
    let rdotstmt_node = Stmt_node ((Simple_id rdot,None),[]) in
    let subg = {strict = false; kind = Digraph; id = Some (Simple_id name); stmt_list = (!cfg_dot_list@[rdotstmt_node])} in
    let () = print_file ((("output/" ^ name) ^ (string_of_int !fcounter)) ^ ".dot") subg in
    let () = print_file ((("output1/" ^ name) ^ (string_of_int !fcounter)) ^ ".dot") subg in
    let ret = {sub_id = Some id; sub_stmt_list = !cfg_dot_list} in
    (* let call_graph = {sub_id = Some id; sub_stmt_list = [Stmt_node ((id,None),[])]} in *)
    let call_graph_id = (id,None) in
    (ret,call_graph_id)
  | Null -> raise (Internal_compiler_error "Hit a Null type topnode, how come??")
							   

let rec dot_filternode = function
  | Filternode (topnode, filternode_list) -> 
    let ll = List.map (fun (x,y) -> (Edge_subgraph x , Edge_subgraph y)) (List.map (fun x -> dot_filternode x) filternode_list) in
    let filternode_subgraphs  = List.map (fun x -> match x with (x,y) -> x) ll in
    let call_graphs = List.map (fun x -> match x with (x,y) -> y) ll in
    let (topnode_subgraph, topnode_call_graph) = (match (dot_topnode topnode) with (x,y) -> (Edge_subgraph x,Edge_node_id y)) in
    let ret = {sub_id = Some (Simple_id (("Filter_node" ^ (string_of_int !counter)))); sub_stmt_list = [Stmt_edge (topnode_subgraph, filternode_subgraphs, []) ]} in
    let call_graph = {sub_id = Some (Simple_id (("Filter_node_call_graph" ^ (string_of_int !counter))));
    		      sub_stmt_list = [Stmt_edge (topnode_call_graph, call_graphs, []) ]} in (ret, call_graph)

let build_program_dot fnode filename =
  let (top_sub_graph, top_call_graph) = dot_filternode fnode in
  let program_graph = {strict = false; kind = Digraph; id = Some (Simple_id "Program_graph"); stmt_list = [Stmt_subgraph top_sub_graph]} in
  let program_call_graph = {strict = false; kind = Digraph; id = Some (Simple_id "Program_graph_call_graph"); stmt_list = [Stmt_subgraph top_call_graph]} in
  (*Now print the graph to the file *)
  print_file filename program_graph;
  print_file (filename ^ "_call_graph.dot") program_call_graph
