open Language
open Language
open StreamGraph
open Odot

exception Error of string
exception Internal_compiler_error of string

let counter = ref 0
let visited_nodes = Hashtbl.create 50

let get_symbol = function
  | Symbol (x,_) -> x

let get_addressed_symbol = function
  | AddressedSymbol (x,_,_,_) -> get_symbol x

let get_typed_symbol = function
  | SimTypedSymbol (_,x,_) -> get_symbol x
  | ComTypedSymbol (_,x,_) -> get_addressed_symbol x
    
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
  | ColonExpr (x,y,z,_) -> ((dot_simpleexpr x) ^ (dot_simpleexpr y)) ^ (dot_simpleexpr z)
  | Opposite (x,_) -> "-" ^ dot_simpleexpr x
  | Brackets (x,_) -> dot_simpleexpr x
  | Cast (x,y,_) -> (("(" ^ DataTypes.print_datatype x) ^ ")") ^ (dot_simpleexpr y)

and get_addressed_string = function
  | AddressedSymbol (x,_,z,_) -> 
    let name = get_symbol x in
    let string_brac_dims = get_string_brac_dims z in
    name ^ string_brac_dims
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
  | Assign (x,y,_) -> 
    let rvalue = (dot_expr y) in
    let lvalue = (( "(" ^ (dot_allsym_list x)) ^ ")") in
    (lvalue ^ " = " ) ^ rvalue
  | VarDecl (x,_) -> (dot_typed_symbol x)
  | Block (x,_) -> 
    let ll = List.map (fun x -> dot_stmt x) x in
    let tot = ref "" in
    let () = (List.iter (fun x -> tot := ((!tot ^ x)) ) ll ) in
    (("{" ^  !tot) ^ "}")
  | Escape (x,_) -> x
  | Noop -> "Noop"
  | For (x,y,z,_) -> 
    let e1 = get_symbol x in
    let e2 = dot_simpleexpr y in
    let e3 = dot_stmt z in
    (" For ") ^ (e1 ^ " " ) ^ (e2 ^ " ") ^ (e3)
  | Par (x,y,z,_) -> 
    let e1 = get_symbol x in
    let e2 = dot_simpleexpr y in
    let e3 = dot_stmt z in
    " Par " ^ (e1 ^ " " ) ^ (e2 ^ " ") ^ (e3)
  | CaseDef (x,_) -> "CaseDef " ^ dot_case x
  | Split (x,_) -> "Split " ^ dot_stmt x

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

let get_edge_child = function
  | Edge (_,x) -> x

let get_edge_weight = function
  | Edge (w,_) -> (Simple_id ("weight"), Some (Simple_id (string_of_int (match w with | Some x -> x | None -> 1))))

let rec dot_stream_graph ll = function
  | TaskSplit (x,num_instr,num_vec,y) as s ->
    (try
       let () = IFDEF DEBUG THEN print_endline ("trying to find TaskSplit " ^ (dot_stmt x) ^ " in hashtbl") ELSE () ENDIF in
       let ret = Hashtbl.find visited_nodes s in
       let () = IFDEF DEBUG THEN print_endline ("found TaskSplit " ^ (dot_stmt x) ^ " in hashtbl") ELSE () ENDIF in ret
     with
       | Not_found -> 
	 let () = IFDEF DEBUG THEN print_endline ("building TaskSplit " ^ (dot_stmt x)) ELSE () ENDIF in
	 let cfg_node_list = List.map(fun x -> dot_stream_graph ll (get_edge_child x)) y in
	 counter := !counter + 1;
	 let this_id = (Simple_id ("TaskSplit" ^ (string_of_int !counter)), None) in
	 let this_label = ("\""^ (dot_stmt x)) ^ "\"" in
	 let constraints = "\"" ^ (string_of_int num_instr) ^"," ^ (string_of_int num_vec) ^ "\"" in
	 let this = Stmt_node (this_id , [(Simple_id("type"), Some (Simple_id("task_split")));
	   (Simple_id("shape"), Some (Simple_id("box")));
					 (Simple_id("Id"),Some(Simple_id((string_of_int !counter))));
					  (Simple_id("label"),Some(Simple_id(this_label)));
					  (* (Simple_id("num_instr"),Some(Simple_id(string_of_int num_instr))); *)
					  (Simple_id("constraints"),Some(Simple_id(constraints)))]) in 
	 let tnode_list = List.map (fun x -> get_tnode_id x) cfg_node_list in
	 let edge_node_ids = List.map (fun x -> Edge_node_id x)  tnode_list in
	 let edge_attrs = List.map (fun x -> get_edge_weight x) y in
	 let rets = List.map2 (fun x y -> Stmt_edge ((Edge_node_id this_id),  [x], [y])) edge_node_ids edge_attrs in
	 ll := this :: !ll;
	 ll := rets @ !ll;
	 Hashtbl.add visited_nodes s (List.hd rets);
	 List.hd rets)

  | Store (sym,y) as s ->
    (try
       let ret = Hashtbl.find visited_nodes s in
       let () = IFDEF DEBUG THEN print_endline ("found Store " ^ (dot_typed_symbol sym) ^ " in hashtbl") ELSE () ENDIF in ret
     with
       | Not_found -> 
	 let () = IFDEF DEBUG THEN print_endline ("building Store " ^ (dot_typed_symbol sym)) ELSE () ENDIF in
	 let cfg_node_list = List.map(fun x -> dot_stream_graph ll (get_edge_child x)) y in
	 let () = IFDEF DEBUG THEN print_endline ("SIZE: " ^ (string_of_int (List.length cfg_node_list))) ELSE () ENDIF in
	 counter := !counter + 1;
	 let this_id = (Simple_id ("Store" ^ (string_of_int !counter)), None) in
	 let this_label = ("\""^ (dot_typed_symbol sym)) ^ "\"" in
	 let this = Stmt_node (this_id , [(Simple_id("type"), Some (Simple_id("store")));(Simple_id("shape"), Some (Simple_id("box")));
					 (Simple_id("Id"),Some(Simple_id((string_of_int !counter))));
					  (Simple_id("label"),Some(Simple_id(this_label)))]) in 
	 let tnode_list = List.map (fun x -> get_tnode_id x) cfg_node_list in
	 let edge_node_ids = List.map (fun x -> Edge_node_id x)  tnode_list in
	 let edge_attrs = List.map (fun x -> get_edge_weight x) y in
	 let () = IFDEF DEBUG THEN print_endline ("SIZE: " ^ (string_of_int (List.length edge_attrs))) ELSE () ENDIF in
	 let rets = List.map2 (fun x y -> Stmt_edge ((Edge_node_id this_id),  [x], [y])) edge_node_ids edge_attrs in
	 ll := this :: !ll;
	 ll := rets @ !ll;
	 Hashtbl.add visited_nodes s (List.hd rets);
	 List.hd rets)

  | Seq (x,num_instr,num_vec,y) as s -> 
    (try
       let ret = Hashtbl.find visited_nodes s in
       let () = IFDEF DEBUG THEN print_endline ("found Seq " ^ (dot_stmt x) ^ " in hashtbl") ELSE () ENDIF in ret
     with
       | Not_found -> 
	 let () = IFDEF DEBUG THEN print_endline ("building Seq " ^ (dot_stmt x)) ELSE () ENDIF in
	 let cfg_node = dot_stream_graph ll (get_edge_child y) in
	 let () = IFDEF DEBUG THEN print_endline "return seq1" ELSE () ENDIF in
	 counter := !counter + 1;
	 let this_id = (Simple_id ("Seq" ^ (string_of_int !counter)), None) in
	 let this_label = ("\""^ (dot_stmt x)) ^ "\"" in
	 let constraints = "\"" ^ (string_of_int num_instr) ^"," ^ (string_of_int num_vec) ^ "\"" in
	 let this = Stmt_node (this_id, [(Simple_id("type"), Some (Simple_id("seq")));(Simple_id("shape"),Some(Simple_id("box")));
					 (Simple_id("label"),Some(Simple_id(this_label)));
					 (Simple_id("Id"),Some(Simple_id((string_of_int !counter))));
					 (* (Simple_id("num_instr"),Some(Simple_id(string_of_int num_instr))); *)
					 (Simple_id("constraints"),Some(Simple_id(constraints)))]) in
	 let tnode = get_tnode_id cfg_node in
	 let edge_attrs = List.map (fun x -> get_edge_weight x) [y] in
	 let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id tnode], edge_attrs) in
	 ll := this :: !ll;
	 ll := ret :: !ll;
	 Hashtbl.add visited_nodes s ret;
	 let () = IFDEF DEBUG THEN print_endline "return seq" ELSE () ENDIF in
	 ret)

  | TaskJoin (x,num_instr,num_vec,y) as s -> 
    (try
       let () = IFDEF DEBUG THEN print_endline ("trying to find TaskJoin " ^ (dot_stmt x) ^ " in hashtbl") ELSE () ENDIF in
       let ret = Hashtbl.find visited_nodes s in
       let () = IFDEF DEBUG THEN print_endline ("found TaskJoin " ^ (dot_stmt x) ^ " in hashtbl") ELSE () ENDIF in ret
     with
       | Not_found ->
	 let () = IFDEF DEBUG THEN print_endline ("building TaskJoin " ^ (dot_stmt x)) ELSE () ENDIF in
	 let cfg_node = dot_stream_graph ll (get_edge_child y) in
	 counter := !counter + 1;
	 let this_id = (Simple_id ("TaskJoin" ^ (string_of_int !counter)), None) in
	 let this_label = ("\""^ (dot_stmt x)) ^ "\"" in
	 let constraints = "\"" ^ (string_of_int num_instr) ^"," ^ (string_of_int num_vec) ^ "\"" in
	 let this = Stmt_node (this_id, [(Simple_id("type"), Some (Simple_id("task_join")));(Simple_id("shape"),Some(Simple_id("box")));
					 (Simple_id("label"),Some(Simple_id(this_label)));
					 (Simple_id("Id"),Some(Simple_id((string_of_int !counter))));
					 (* (Simple_id("num_instr"),Some(Simple_id(string_of_int num_instr))); *)
					 (Simple_id("constraints"),Some(Simple_id(constraints)))]) in
	 let tnode = get_tnode_id cfg_node in
	 let edge_attrs = List.map (fun x -> get_edge_weight x) [y] in
	 let ret = Stmt_edge ((Edge_node_id this_id),  [Edge_node_id tnode], edge_attrs) in
	 ll := this :: !ll;
	 ll := ret :: !ll;
	 Hashtbl.add visited_nodes s ret;
	 ret)


  | EmptyActor as s -> 
    (try
       let () = IFDEF DEBUG THEN print_endline ("trying to find Empty node in hashtbl") ELSE () ENDIF in
       let ret = Hashtbl.find visited_nodes s in
       let () = IFDEF DEBUG THEN print_endline ("found Empty node in hashtbl") ELSE () ENDIF in ret
    with
      | Not_found ->
	let () = IFDEF DEBUG THEN print_endline "building Empty Actor" ELSE () ENDIF in
	counter := !counter + 1;
	let this_id = (Simple_id("EmptyActor" ^ (string_of_int !counter)), None) in
	let ret = Stmt_node (this_id, []) in
	Hashtbl.add visited_nodes s ret;
	ret)

let build_program_dot filename fnode =
  let stmt_list = ref [] in
  let _ = dot_stream_graph stmt_list fnode in
  (* Building program graph !! *)
  let () = IFDEF DEBUG THEN print_endline "printing out the dot file!!" ELSE () ENDIF in
  let program_graph = {strict = false; kind = Graph; id = Some (Simple_id "Stream_graph"); stmt_list = !stmt_list} in
  print_file filename program_graph;
