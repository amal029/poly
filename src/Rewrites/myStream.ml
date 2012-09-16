open Language
open Language
open StreamGraph

(*

  IMP: This file parses the AST constructed after decompiling the
  intermediate CFG Graph !!

  Purpose: This file compiles the Pico programs into stream graphs Next,
  the stream graphs are projected onto the CFG and LLVM backends for
  profiling. Finally, the graph partitoning algorithm is used to
  partition the streams graphs onto heterogeneous CPU/GPU mixtures.
  
  Once that is done, we then combine it with the full program polyhedral
  data-structures and generate partitioned code for LLVM backend.

  Author: Avinash Malik

  Sun Aug 12 14:55:37 IST 2012

*)

module List = Batteries.List
module Array = Batteries.Array

exception Error of string
exception Internal_compiler_error of string

let graph_tile = ref false
let counter = ref 0

(* Currently the simple expressions in a loop are considered constant *)
let rec get_const_value s lc = function
  | Const (_,x,_) -> int_of_string x
  | Cast (_,x,_) ->  get_const_value s lc x
  | Brackets (x,_) -> get_const_value s lc x
  | Opposite (x,_) -> get_const_value s lc x
  | _ as s1 -> raise 
    (Error ((Reporting.get_line_and_column lc) ^ "currently loop expressions need to be resolved to a constant only, I got: " ^ Dot.dot_simpleexpr s1 ^ " in " ^ 
     Dot.dot_simpleexpr s))

let get_new_num_instr lc curr = function
  | ColonExpr (x,y,z,lc) as s -> 
    let stride = get_const_value s lc z in
    let start = get_const_value s lc x in
    let bound = get_const_value s lc y in
    abs (curr * (((bound - start)/stride)+1))
  | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " loop expression not a colon-expression"))

(* let set_edge_parent parent = function *)
(*   | Edge (x,_,_) -> x := parent *)

let get_edge_child = function
  | Edge (_,z) -> z

(* let get_edge_parent = function *)
(*   | Edge (x,_,_) -> !x *)

let get_edge_weight = function
  | Edge (w,_) -> w

let get_nedge_child = function
  | Edge (_,x) -> x

let rec ndebug = function
  | Store (t,x) -> print_endline (Dot.dot_typed_symbol t); List.iteri (fun c x -> print_endline ("Child: " ^ (string_of_int c)); ndebug (get_nedge_child x)) x
  | Seq (x,_,_,c) -> print_endline ((Dot.dot_stmt x) ^ "Child: 0"); ndebug (get_nedge_child c)
  | TaskSplit (x,y,z,c) -> print_endline ("Task split: " ^ (Dot.dot_stmt x)); List.iteri (fun c x -> print_endline ("Child: " ^ (string_of_int c)); 
    ndebug (get_nedge_child x)) c
  | TaskJoin (x,y,z,c) -> print_endline ("Task Join: " ^ ((Dot.dot_stmt x) ^ "Child: 0")); ndebug (get_nedge_child c)
  | EmptyActor -> print_endline "Empty actor"

(* For debugging *)
let rec debug = function
  | Store (t,x) -> print_endline (Dot.dot_typed_symbol t); List.iter (fun x -> debug (get_edge_child x)) x
  | Seq (x,_,_,c) -> print_endline (Dot.dot_stmt x); debug (get_edge_child c)
  | TaskSplit (x,y,z,c) -> print_endline ("Task split: " ^ (Dot.dot_stmt x)); List.iter (fun x -> debug (get_edge_child x)) c
  | TaskJoin (x,y,z,c) -> print_endline ("Task Join: " ^ (Dot.dot_stmt x)); debug (get_edge_child c)
  | EmptyActor -> print_endline "Empty actor"

let change_stmt_location i = function
  | Assign (x,y,(l,c)) -> Assign (x,y,(l, (c+i)))
  | Split (x,(l,c)) -> Split (x,(l, (c+i)))
  | Par (x,y,z,(l,c)) -> Par (x,y,z,(l, (c+i)))
  | Noop -> Escape ("Noop",(i,i))
  | Escape (y,(l,c)) -> Escape (y,(l, (c+i)))
  | CaseDef (x,(l,c)) -> CaseDef (x,(l, (c+i)))
  | _ -> raise (Internal_compiler_error "Incorrect type of stmt when replacing Par stmt columns")

let change_typed_symbol i = function
  | SimTypedSymbol (x,y,(l,c)) -> SimTypedSymbol (x,y,(l,(c+i)))
  | ComTypedSymbol (x,y,(l,c)) -> ComTypedSymbol (x,y,(l,(c+i)))

let rec replace_stmt = function

  | Store (t,x) ->
    counter := !counter + 1;
    let t = change_typed_symbol !counter t in
    let children = List.map(fun x -> replace_stmt (get_edge_child x)) x in
    let weights = List.map (fun x -> get_edge_weight x) x in
    let edges = List.map2 (fun w c -> Edge (w, c)) weights children in
    Store (t,edges)

  | Seq (x,y,z,c) -> 
    counter := !counter + 1;
    let x = change_stmt_location !counter x in
    let weight = get_edge_weight c in
    let c2 = replace_stmt (get_edge_child c) in
    let edge = Edge (weight, c2) in
    let ret = Seq (x,y,z,edge) in (* set_edge_parent ret edge; *) ret

  | TaskSplit (x,y,z,c) ->
    counter := !counter + 1;
    let x = change_stmt_location !counter x in
    let children = List.map(fun x -> replace_stmt (get_edge_child x)) c in
    let weights = List.map (fun x -> get_edge_weight x) c in
    let edges = List.map2 (fun w c -> Edge (w, c)) weights children in
    let ret = TaskSplit(x,y,z,edges) in ret

  | TaskJoin (x,y,z,c) -> 
    counter := !counter + 1;
    let x = change_stmt_location !counter x in
    let weight = get_edge_weight c in
    let c2 = replace_stmt (get_edge_child c) in
    let edge = Edge (weight, c2) in
    let ret = TaskJoin(x,y,z,edge) in (* set_edge_parent ret edge; *) ret

  | EmptyActor as s -> s

let rec replace_empty_actor child = function
  | Store (t,x) -> 
    let children = List.map(fun x -> replace_empty_actor child (get_edge_child x)) x in
    let weights = List.map (fun x -> get_edge_weight x) x in
    let edges = List.map2 (fun w c -> Edge (w, c)) weights children in
    let ret = Store (t,edges) in ret
    (* let () = List.iter (fun x -> set_edge_parent ret x) edges in ret *)
  | Seq (x,y,z,c) -> 
    let weight = get_edge_weight c in
    let c2 = replace_empty_actor child (get_edge_child c) in
    let edge = Edge (weight, c2) in
    let ret = Seq (x,y,z,edge) in (* set_edge_parent ret edge; *) ret
  | TaskSplit (x,y,z,c) -> 
    let children = List.map(fun x -> replace_empty_actor child (get_edge_child x)) c in
    let weights = List.map (fun x -> get_edge_weight x) c in
    let edges = List.map2 (fun w c -> Edge (w, c)) weights children in
    let ret = TaskSplit(x,y,z,edges) in ret
    (* let () = List.iter (fun x -> set_edge_parent ret x) edges in ret *)
  | TaskJoin (x,y,z,c) -> 
    let weight = get_edge_weight c in
    let c2 = replace_empty_actor child (get_edge_child c) in
    let edge = Edge (weight, c2) in
    let ret = TaskJoin(x,y,z,edge) in (* set_edge_parent ret edge; *) ret
  | EmptyActor -> 
    let () = IFDEF DEBUG THEN print_endline "replacing empty actor with child" ELSE () ENDIF in
    child

let get_symbol = function
  | Symbol (x,_) -> x
let get_addressed_symbol = function
  | AddressedSymbol (x,_,_,_) -> get_symbol x
let get_typed_symbol = function
  | SimTypedSymbol (_,x,_) -> get_symbol x
  | ComTypedSymbol (_,x,_) -> get_addressed_symbol x

let rec get_simexpr sym = function
  | Plus(x,y,_) | Minus(x,y,_) | Times(x,y,_) | Div(x,y,_) 
  | Mod (x,y,_) | Pow (x,y,_) | Rshift (x,y,_) | Lshift (x,y,_) -> get_simexpr sym x || get_simexpr sym y
  | Const _ | TStar | TStarStar -> false
  | Brackets (x,_) -> get_simexpr sym x
  | Cast (_,x,_) -> get_simexpr sym x
  | Opposite (x,_) -> get_simexpr sym x
  | VarRef (x,_) -> get_symbol x = get_typed_symbol sym
  | AddrRef (x,_) -> get_addressed_symbol x = get_typed_symbol sym
  | ColonExpr (x,y,z,_) -> get_simexpr sym x || get_simexpr sym y || get_simexpr sym z

let get_callarg sym = function
  | CallAddrressedArgument x -> get_addressed_symbol x = get_typed_symbol sym
  | CallSymbolArgument x -> get_symbol x = get_typed_symbol sym

let rec get_callargument_list sym = function
  | h::t -> get_callarg sym h || get_callargument_list sym t
  | [] -> false

let get_fcall sym = function
  | Call (_,x,_) -> get_callargument_list sym x

let get_rvalue sym = function
  | FCall (x,_) -> get_fcall sym x
  | SimExpr x -> get_simexpr sym x

let get_allsym sym = function
  | AllAddressedSymbol x -> get_addressed_symbol x = get_typed_symbol sym
  | AllSymbol x -> get_symbol x = get_typed_symbol sym
  | AllTypedSymbol x -> get_typed_symbol x = get_typed_symbol sym

let rec get_lvalue sym = function
  | h::t -> get_allsym sym h || get_lvalue sym t
  | [] -> false

let get_elements_in_simple_expr lc = function
  | Const (_,x,_) -> int_of_string x
  | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " ComTyped Symbol does not have constant dimensions"))

let get_elements lc = function
  | BracDim x -> List.fold_right (fun x y -> (match x with DimSpecExpr x -> (get_elements_in_simple_expr lc x) + y)) x 0

let get_element_list lc = function
  | BracDim x -> List.map (fun (DimSpecExpr x) -> get_elements_in_simple_expr lc x) x

let get_element_list_size lc = function
  | BracDim x -> List.length x

let get_total_size_ngt = function
  | ComTypedSymbol (typ,x,lc) as s -> 
    let size = DataTypes.getdata_size typ in
    let num_elements = (match x with | AddressedSymbol (_,_,x,_) -> List.fold_right (fun x y -> 
      let el = get_elements lc x in 
      let () = IFDEF DEBUG THEN print_endline ((string_of_int el) ^ "$$$$$$$") ELSE () ENDIF in
      el + y) x 0) in
    size * num_elements
  | SimTypedSymbol (typ,_,_) -> DataTypes.getdata_size typ

let get_total_size = function
  | ComTypedSymbol (typ,x,lc) as s -> 
    let size = DataTypes.getdata_size typ in
    let num_elements = (match x with | AddressedSymbol (_,_,x,_) -> List.fold_right (fun x y -> 
      let el = get_elements lc x in 
      let () = IFDEF DEBUG THEN print_endline ((string_of_int el) ^ "$$$$$$$") ELSE () ENDIF in
      el + y) x 0) in
    size * num_elements
  | SimTypedSymbol _ -> raise (Internal_compiler_error "Cannot hit simtyped symbol when getting total size for a comtyped symbol")

let get_addressed_symbol_used_size sym = function
  | AddressedSymbol (_,_,ll,mlc) ->
    let (typ,symlist,lc) = (match sym with ComTypedSymbol (typ,x,_) -> (match x with AddressedSymbol (_,_,ll,lc) -> (typ,ll,lc))) in
    let symlist = get_element_list lc (List.hd symlist) in
    let mylistsize = get_element_list_size lc (List.hd ll) in
    let usedlist = List.drop mylistsize symlist  in
    if usedlist = [] then DataTypes.getdata_size typ
    else (List.fold_right (+) usedlist 0) * (DataTypes.getdata_size typ)

let rec get_simexpr_size sym = function
  | Plus(x,y,_) | Minus(x,y,_) | Times(x,y,_) | Div(x,y,_) 
  | Mod (x,y,_) | Pow (x,y,_) | Rshift (x,y,_) | Lshift (x,y,_) -> get_simexpr_size sym x + get_simexpr_size sym y
  | Const _ | TStar | TStarStar -> 0
  | Brackets (x,_) -> get_simexpr_size sym x
  | Cast (_,x,_) -> get_simexpr_size sym x
  | Opposite (x,_) -> get_simexpr_size sym x
  | VarRef (x,_) -> get_total_size sym
  | AddrRef (x,_) -> get_addressed_symbol_used_size sym x
  | ColonExpr (x,y,z,_) -> get_simexpr_size sym x + get_simexpr_size sym y + get_simexpr_size sym z

let get_callarg_size sym = function
  | CallAddrressedArgument x -> get_addressed_symbol_used_size sym x
  | CallSymbolArgument x -> get_total_size sym

let rec get_callargument_list_size sym = function
  | h::t -> get_callarg_size sym h + get_callargument_list_size sym t
  | [] -> 0

let get_fcall_size sym = function
  | Call (_,x,_) -> get_callargument_list_size sym x

let get_rvalue_size sym = function
  | FCall (x,_) -> get_fcall_size sym x
  | SimExpr x -> get_simexpr_size sym x

let get_lvalue_size sym = function
  | AllSymbol _ | AllTypedSymbol _ -> get_total_size sym
  | AllVecSymbol _ -> raise (Internal_compiler_error "Cannot yet handle a vectorized statement when performing graph based tiling")
  | AllAddressedSymbol x -> get_addressed_symbol_used_size sym x

let get_used_size sym = function
  | Assign (x,y,_) as s -> (List.fold_right (fun x t -> (get_lvalue_size sym x) + t) x 0) + get_rvalue_size sym y
  | Noop -> 0
  | Escape _ -> 0
  | _ as s -> raise (Internal_compiler_error ("erroneously obtained: " ^ Dot.dot_stmt s))

let get_dep_actor sym = function
  | Assign (x,y,_) as s -> 
    let () = IFDEF DEBUG THEN print_endline "Checking for connection to store" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline (Dot.dot_stmt s) ELSE () ENDIF in
    let ret = get_lvalue sym x || get_rvalue sym y in
    let () = IFDEF DEBUG THEN if ret then print_endline ("Will connect " ^ (Dot.dot_typed_symbol sym) ^ " to this stmt") ELSE () ENDIF in 
    ret
  | Noop -> false
  | Escape _ -> false
  | _ as s -> raise (Internal_compiler_error ("erroneously obtained: " ^ (Dot.dot_stmt s)))

let rec get_dependence_actors ret sym = function
  | TaskJoin (_,_,_,x) -> get_dependence_actors ret sym (get_edge_child x)
  | Store (_,x) -> List.iter (fun x -> get_dependence_actors ret sym (get_edge_child x)) x
  | TaskSplit (_,_,_,x) -> List.iter (fun x -> get_dependence_actors ret sym (get_edge_child x)) x
  | Seq (t,_,_,x) as s -> 
    (* Check that we don't already have a connection in the list*)
    (if not (List.exists ((=) s) !ret) then
	if get_dep_actor sym t then 
	  let () = IFDEF DEBUG THEN print_endline (("*****") ^ Dot.dot_typed_symbol sym ^ ("*******")) ELSE () ENDIF in
	  ret := s :: !ret; else ());
    get_dependence_actors ret sym (get_edge_child x)
  | EmptyActor -> ()

let get_composite_size actor = function
  | SimTypedSymbol (typ,_,_) -> DataTypes.getdata_size typ
  | ComTypedSymbol (typ,x,lc) as s -> 
    let stmt = (match actor with Seq (s,_,_,_) -> s | _ -> raise (Internal_compiler_error "Stores can only be used in Seq statements")) in
    get_used_size s stmt

let make_dependece_edges tsym list = 
  (* First build the edges *)
  (* Calculate the size of the array *)
  if (match list with | [] -> false | _ -> true) then
    if !graph_tile then
      let sizes = List.map (fun x -> get_composite_size x tsym) list in
      (* let size = get_composite_size tsym in *)
      let edges = List.map2 (fun x y -> Edge (Some y, x)) list sizes in
      (* Now replace the parent with the store *)
      let ret = Store (tsym, edges) in ret
    else
      let size = get_total_size_ngt tsym in
      let edges = List.map (fun x -> Edge (Some size, x)) list in
      let ret = Store (tsym, edges) in ret
  (* let () = List.iter (fun x -> set_edge_parent ret x) edges in ret *)
  else EmptyActor

let make_edge tsym list node = 
  let () = IFDEF DEBUG THEN print_endline ("calling 1 with " ^ (Dot.dot_typed_symbol tsym)) ELSE () ENDIF in
  let () = get_dependence_actors list tsym node in
  let () = IFDEF DEBUG THEN print_endline "calling 2" ELSE () ENDIF in
  let ret = make_dependece_edges tsym !list in
  list := []; ret

let rec process_filter filters num_instr num_vec = function
  | Filter (x,y,z,stmt) as s ->
    (* First get all the declarations from the arguments declared with this filter 
       and put them into the declarations list ref *)
    let declarations = ref (y@z) in
    let node = process_list declarations filters num_instr num_vec [stmt] in
    (* Now get all the actors in the stream graph that are using the declarations *)
    (* and make the dependence graph between the store and the collected actors *)
    let llist = ref [] in
    (* let stores = List.map (fun x -> make_dependece_edges x (let () = get_dependence_actors llist x node in !llist)) !declarations in *)
    let stores = List.map (fun x -> make_edge x llist node) !declarations in
    (* Return all the things back for further processing *)
    let node_edge = Edge(None, node) in
    let store_edges = List.map (fun x -> Edge(None, x)) stores in
    let edges = node_edge :: store_edges in
    let ret = TaskSplit (stmt,0,0,edges) in ret

and process_stmt declarations list filters num_instr num_vec = function

  | Assign (sts,x,lc) as s ->
    (* Here we need to check if a call is being made to a different filter!! *)
    (* Get all the declaration types *)
    declarations := !declarations @ List.map (fun (AllTypedSymbol x) -> x)
      (List.filter (fun x -> (match x with | AllTypedSymbol _ -> true | _ -> false)) sts);
    let child = process_list declarations filters num_instr num_vec list in
    (match x with
      | FCall (x,b) ->
	(* If this is the case then we need to go into the filter and make its node *)
	(try 
	   let ret = process_filter filters num_instr num_vec (List.find (fun x1 -> (match x with | Call (x,_,_) -> x = (match x1 with Filter(x,_,_,_) -> x))) filters) in
	   let () = IFDEF DEBUG THEN print_endline "replacing task_parallel empty actor" ELSE () ENDIF in
	   let ret = replace_empty_actor ret child in 
	   let () = IFDEF DEBUG THEN print_endline "replaced" ELSE () ENDIF in
	   ret
	 with Not_found -> 
	   if b then
	     let edge = Edge (None, child) in let ret = Seq (s,num_instr,num_vec,edge) in (* let () = set_edge_parent ret edge in *) ret
	   else 
	     let fname = (match x with | Call (x,_,_) -> x) in
	     raise (Error ((Reporting.get_line_and_column lc) ^ " filter named:" ^ get_symbol fname ^ " is unbound!!")))
      | _ -> let edge = Edge (None, child) in let ret = Seq (s,num_instr,num_vec,edge) (* let () = set_edge_parent ret edge *) in ret)
  (* Need to take care of vardecl --> make it into a store node later on !! *)

  | VarDecl (x,_) -> 
    declarations := !declarations @ [x];
    process_list declarations filters num_instr num_vec list

  | Escape _ | Noop as s ->
    counter := !counter + 1;
    let s = change_stmt_location !counter s in
    let child = process_list declarations filters num_instr num_vec list in
    let edge = Edge (None, child) in
    let ret = Seq (s,num_instr,num_vec,edge) in ret
    (* let () = set_edge_parent ret edge in ret *)

  | CaseDef (x,lc) as s -> 
  (* Build a task parallel actor, because every branch of if-else is
     separate of each other*)
    let child = process_list declarations filters num_instr num_vec list in
    let stmts = (match x with | Case (x,y,_) -> (List.map (fun x -> match x with Clause (_,x,_) -> x) x) @ [(match y with | Otherwise (x,_) -> x)]) in
    let mes = List.map (fun x -> process_stmt declarations [] filters num_instr num_vec x) stmts in
    let () = IFDEF DEBUG THEN List.iter (fun x -> debug x; print_endline "NEXT") mes ELSE () ENDIF in
    let join_edge = Edge (None, child) in
    let myjoin = TaskJoin (s,num_instr,num_vec,join_edge) in
    (* let () = set_edge_parent myjoin join_edge in *)
    let mes = List.map (fun x -> replace_empty_actor myjoin x) mes in
    let () = IFDEF DEBUG THEN List.iter (fun x -> debug x; print_endline "NEXT 2") mes ELSE () ENDIF in
    let edge_list = List.map (fun x -> Edge (None, x)) mes in
    let ret = TaskSplit (s,num_instr,num_vec,edge_list) in
    (* let () = List.iter (fun x -> set_edge_parent ret x) edge_list in  *)
    let () = IFDEF DEBUG THEN debug ret; print_endline "NEXT 3" ELSE () ENDIF in
    ret

  | Block (x,lc) -> 
    (* Every block needs to be done separately *)
    let child = process_list declarations filters num_instr num_vec list in
    let me = process_list declarations filters num_instr num_vec x in
    replace_empty_actor child me

  | For (x,y,stmt,lc) -> 
    (* A for loop only increments num_instr *)
    (* FIXME: for now we consider the loop bounds to be constants, but
       this can be relaxed with some effort in the compiler*)
    let child = process_list declarations filters num_instr num_vec list in
    let ni = get_new_num_instr lc num_instr y in
    let () = IFDEF DEBUG THEN print_endline ("For: " ^ (string_of_int ni)) ELSE () ENDIF in
    let me = process_stmt declarations [] filters ni num_vec stmt in
    (* Replace the end of me *)
    replace_empty_actor child me 

  | Par (x,y,stmt,lc) as s -> 
    (* Here we do the exact same as the for stmt, but increment the num_vec instead *)
    let child = process_list declarations filters num_instr num_vec list in
    let give = if num_vec = 0 then 1 else num_vec in
    let nv = get_new_num_instr lc give y in
    if not !graph_tile then
      let () = IFDEF DEBUG THEN print_endline ("Par: " ^ (string_of_int nv)) ELSE () ENDIF in
      let me = process_stmt declarations [] filters num_instr nv stmt in
      replace_empty_actor child me
    else
    (* The code below produces the graph, but the number of edges do not match: FIXME*)
      let () = IFDEF DEBUG THEN print_endline "Making tiled graph" ELSE () ENDIF in
      let mes = Array.init nv (fun i -> process_stmt declarations [] filters num_instr give stmt) in
      let mes = Array.map replace_stmt mes in
      let mes = Array.to_list mes in
      let join_edge = Edge (None, child) in
      let myjoin = TaskJoin (s,0,0,join_edge) in
      let mes = List.map (fun x -> replace_empty_actor myjoin x) mes in
      let edge_list = List.map (fun x -> Edge (None, x)) mes in
      let ret = TaskSplit (s,0,0,edge_list) in ret

  | Split (x,lc) as s -> 
    let child = process_list declarations filters num_instr num_vec list in
    let stmts = (match x with | Block (x,_) -> x | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " Split is not of Block type!!"))) in
    let mes = List.map (fun x -> process_stmt declarations [] filters num_instr num_vec x) stmts in
    let join_edge = Edge (None, child) in
    let myjoin = TaskJoin (s,num_instr,num_vec,join_edge) in
    (* set_edge_parent myjoin join_edge; *)
    let mes = List.map (fun x -> replace_empty_actor myjoin x) mes in
    let edge_list = List.map (fun x -> Edge (None, x)) mes in
    let ret = TaskSplit (s,num_instr,num_vec,edge_list) in ret
    (* let () = List.iter (fun x -> set_edge_parent ret x) edge_list in ret *)

and process_list declarations filters num_instr num_vec = function
  | h::t -> process_stmt declarations t filters num_instr num_vec h
  | [] -> EmptyActor

let rec process_main filters = function
  | DefMain (x,y,lc) -> process_filter filters 1 0 x

let rec convert_to_our_metis_graph = function
  | TaskSplit (x,y,z,r) ->
    (* let z = if z = 0 then 1 else z in *)
    Metis.Split ((Dot.dot_stmt x), [y;z], List.map (fun x -> convert_to_our_metis_graph_edge x) r)
  | EmptyActor -> Metis.Empty
  | Store (x,y) -> Metis.Split ((Dot.dot_typed_symbol x), [0;0], List.map (fun x -> convert_to_our_metis_graph_edge x) y)
  | Seq (x,y,z,e) ->
    (* let z = if z = 0 then 1 else z in *)
    Metis.Seq ((Dot.dot_stmt x), [y;z], convert_to_our_metis_graph_edge e)
  | TaskJoin (x,y,z,e) -> 
    (* let z = if z = 0 then 1 else z in *)
    Metis.Join ((Dot.dot_stmt x), [y;z],convert_to_our_metis_graph_edge e)
and convert_to_our_metis_graph_edge = function
  | Edge (w,x) -> Metis.Edge (w, convert_to_our_metis_graph x)

let rec convert_to_metis_graph = function
  | TaskSplit (x,y,z,r) -> 
    let z = if z = 0 then 1 else z in
    Metis.Split ((Dot.dot_stmt x), [y*z], List.map (fun x -> convert_to_metis_graph_edge x) r)
  | EmptyActor -> Metis.Empty
  | Store (x,y) -> Metis.Split ((Dot.dot_typed_symbol x), [0], List.map (fun x -> convert_to_metis_graph_edge x) y)
  | Seq (x,y,z,e) -> 
    let z = if z = 0 then 1 else z in
    Metis.Seq ((Dot.dot_stmt x), [y*z], convert_to_metis_graph_edge e)
  | TaskJoin (x,y,z,e) -> 
    let z = if z = 0 then 1 else z in
    Metis.Join ((Dot.dot_stmt x), [y*z],convert_to_metis_graph_edge e)
and convert_to_metis_graph_edge = function
  | Edge (w,x) -> Metis.Edge (w, convert_to_metis_graph x)


let build_stream_graph gt = function
  | Program x ->
    graph_tile := gt;
    counter := 0;
    (* First collect all the filters defined in the program *)
    let filters = List.find_all (fun x -> (match x with Def _ -> true | _ -> false)) x in
    let filters = List.map (fun x -> (match x with | Def (x,_,_) -> x)) filters in
    (* Now process the stream graph starting from the main filter, just like FCFG is built *)
    (try 
       let main = List.find (fun x -> (match x with DefMain _ -> true | _ -> false)) x in
       let ret = process_main filters main in
       let ret2 = convert_to_metis_graph ret in
       let ret3 = convert_to_our_metis_graph ret in
       (* debug *)
       let () = IFDEF DEBUG THEN ndebug ret ELSE () ENDIF in (ret,ret2,ret3)
     with | Not_found -> raise (Error "No main function defined"));
    (* Now start processing from the main filter *)
