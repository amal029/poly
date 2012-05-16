(**
   {1} This module does constant propogation and type-checking
*)
open Language
open Language
open CFG
open Consts

exception Internal_compiler_error of string ;;
exception Error of string;;
exception Nothing

let consts = Hashtbl.create (20);;
let nodes = Hashtbl.create (20);;
let top_nodes = Hashtbl.create (20);;

let get_symbol = function
  | Symbol x -> x

let get_addressed_symbol = function
  | AddressedSymbol (x,_,_) -> get_symbol x

let get_typed_symbol = function
  | SimTypedSymbol (x,y) -> get_symbol y
  | ComTypedSymbol (x,y) -> get_addressed_symbol y


let get_value = function
  | VConst (_,x) -> x
  | _ -> raise (Error ("Value Top undefined or not supported by the native architecure "))

let is_int_type = function
  | DataTypes.Int8
  | DataTypes.Int16
  | DataTypes.Int32
  | DataTypes.Int64
  | DataTypes.Int8s
  | DataTypes.Int16s
  | DataTypes.Int32s
  | DataTypes.Int64s -> true
  | _ -> false

(* Get the biggest type from the unsigned integral list *)
let rec get_index (value:DataTypes.t) counter = function
  | h::t -> if value = h then counter else get_index value (counter+1) t
  | [] -> raise (Error ("Datatype not defined"))

let get_data_type lvalue rvalue =
  if ((List.exists (fun x -> (match x with lvalue -> true )) DataTypes.signed) && (List.exists (fun x -> x = rvalue) DataTypes.unsignedIntegral)) then lvalue
  else if ((List.exists (fun x -> x = rvalue) DataTypes.signed) && (List.exists (fun x -> x = lvalue) DataTypes.unsignedIntegral) ) then rvalue
  else if (get_index lvalue 0 DataTypes.unsignedIntegral) >= (get_index rvalue 0 DataTypes.unsignedIntegral) then lvalue else rvalue

let get_data_type_float lvalue rvalue =
  if (get_index lvalue 0 DataTypes.floating) >= (get_index rvalue 0 DataTypes.floating) then lvalue else rvalue

let process lvalue rvalue func_int func_float = 
  let ttype = (match lvalue with | VConst(x,_) -> x | Top x -> x) in
  if ((match lvalue with | VConst (_,_) -> true | _ -> false)  && (match rvalue with | VConst (_,_) -> true | _ -> false)) then
    if((match lvalue with | VConst (x,_) -> is_int_type x
      | _ -> false)  && 
	  (match rvalue with | VConst (x,_) ->  is_int_type x
	    | _ -> false)) then
      try 
	let value = (func_int (int_of_string (get_value lvalue)) (int_of_string (get_value rvalue))) in
	let dt = get_data_type (match lvalue with | VConst (x,_) -> x | _ -> raise (Failure "")) 
	  (match rvalue with | VConst (x,_) -> x | _ -> raise (Failure "")) in
	VConst (dt, (string_of_int value))
      with
	| Failure _ -> raise (Error (("Value " ^ ((get_value lvalue) ^" or ")) ^ (get_value rvalue ^ "undefined or not supported by the native architecure ")))
    else
      try
	let dt = get_data_type_float (match lvalue with | VConst (x,_) -> x | _ -> raise (Failure "")) 
	  (match rvalue with | VConst (x,_) -> x | _ -> raise (Failure "")) in
	let value = (func_float (float_of_string (get_value lvalue)) (float_of_string (get_value rvalue))) in
	VConst (dt,(string_of_float value))
      with
	| Failure _ -> raise (Error (("Value " ^ ((get_value lvalue) ^" or ")) ^ (get_value rvalue ^ "undefined or not supported by the native architecure ")))
  else Top ttype

let rec get_simexpr_const = function
  | Const (x,y) -> VConst (x,y)
  | VarRef x -> 
    (try 
       Hashtbl.find consts (get_symbol x)
     with | Not_found -> failwith (("Var " ^ (get_symbol x)) ^ " not defined before use"))
  (* Try floding expressions and see if you get a constant *)
  | AddrRef x -> 
    (try 
       Hashtbl.find consts (get_addressed_symbol x)
     with | Not_found -> failwith (("Var " ^ (get_addressed_symbol x)) ^ " not defined before use"))
  | Plus (x,y) ->
    let lvalue = get_simexpr_const x in
    let rvalue = get_simexpr_const y in
    process lvalue rvalue BatInt.add BatFloat.add
  | Minus (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.sub BatFloat.sub
  | Times (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.mul BatFloat.mul
  | Div (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.div BatFloat.div
  | Pow (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.pow BatFloat.pow
  | Cast (d,y) -> 
    let ret = get_simexpr_const y in
    (match ret with | VConst (_,x) -> VConst(d,x) | _ as s -> s)
  | Brackets x | Opposite x -> get_simexpr_const x
  | ColonExpr (x,y,z) -> raise (Error ("Colon expressions not allowed in expressions "))
  | _ -> Top DataTypes.None
(* These are for polyhedrons *)
and get_vardecl_consts = function
  | ComTypedSymbol (x,y) -> 
    let () = get_dim_consts y in
    VConst (x,"NULL")
  | SimTypedSymbol (x,_) -> VConst (x,"NULL")
and get_dim_consts = function
  | AddressedSymbol (_,y,z) -> 
    let () = get_angledim_consts y in
    let () = get_dimspec_consts z in ()
and get_angledim_consts = function
  | h::t -> get_angledimexpr_const h ; get_angledim_consts t
  | [] -> ()
and get_angledimexpr_const = function
  | AngleDimExpr x -> 
    (match x with 
      | DimSpecExpr x ->  
	let ret = get_simexpr_const x in
	(match ret with 
	  | Top _ -> raise (Error "AngleDim Expr not a constant")
	  | VConst(_,x) -> if x = "NULL" then raise (Error "AngleDim Expr is NULL") else ())
    )
and get_dimspec_consts = function
  | h::t-> get_bracdim_const h; get_dimspec_consts t
  | [] -> ()
and get_bracdim_const = function
  | BracDim x -> get_bracdim_consts x
and get_bracdim_consts = function
  | h::t -> get_dimspecexpr_const h; get_bracdim_consts t
  | [] -> ()
and get_dimspecexpr_const = function
  | DimSpecExpr x -> 
    let ret = get_simexpr_const x in
    (match ret with 
      | Top _ -> raise (Error "Dimspec Expr not a constant")
      | VConst(_,x) -> if x = "NULL" then raise (Error "Dimspec Expr is NULL") else ())

let get_expr_const lvalue_assign_type_list = function
  | SimExpr x -> [get_simexpr_const x]
  (* This is fcall *)
  | _ ->  (* FIXME: You need to see how to get the actual value if it is a const from here *)
    List.map (fun x -> Top x) lvalue_assign_type_list

let assign_lvalue_const rvalue = function
  | AllSymbol x -> 
    let ttype = match (Hashtbl.find consts (get_symbol x)) with VConst (x,_) -> x | Top x -> x in
    Hashtbl.replace consts (get_symbol x) (Top ttype)
  | AllTypedSymbol x -> 
    (* Check that dimensions of the addressed symbol are constants *)
    (match x with
      | ComTypedSymbol (_,y) -> get_dim_consts y
      |  _ -> ()
    );
    Hashtbl.add consts (get_typed_symbol x) rvalue 
  | AllAddressedSymbol x -> 
    let ttype = match (Hashtbl.find consts (get_addressed_symbol x)) with VConst (x,_) -> x | Top x -> x in
    Hashtbl.replace consts (get_addressed_symbol x) (Top ttype)

let rec get_assign_lvalue_const counter rvalue = function
  | h::t -> assign_lvalue_const (List.nth rvalue counter) h; get_assign_lvalue_const (counter+1) rvalue t
  | [] -> ()

(* Depends upon the simple type inference engine *)
(* Return a list of DataTypes.t *)
let get_lvalue_type_2 = function
  | AllTypedSymbol (SimTypedSymbol (x,_) | ComTypedSymbol(x,_)) -> x
  | AllAddressedSymbol (AddressedSymbol (x,_,_)) 
  | AllSymbol x -> match (Hashtbl.find consts (get_symbol x)) with VConst (x,_) -> x | Top x -> x
let rec get_lvalue_type = function
  | h::t -> get_lvalue_type_2 h :: get_lvalue_type t
  | [] -> []

let rec get_stmt_const = function
  | VarDecl x -> 
    let sym = (get_typed_symbol x) in
    Hashtbl.add consts sym (get_vardecl_consts x)
  (*FIXME: you are not checking that the assignment is of equal sized
    dimensions, do that --> Done in the type inference engine *)
  | Assign (x,y) ->
    let rvalue =  get_expr_const (get_lvalue_type x) y in
    get_assign_lvalue_const 0 rvalue x
  | Escape x -> ()
  | Noop -> ()
  | _ -> raise (Error ("Unexpected statement encountered after rewrites!!"))


let rec get_dvars = function
  | Block x -> get_bvars x
  | _ -> []
and get_bvars = function
  | h::t -> get_stmt_vars h @ get_bvars t
  | [] -> []
and get_stmt_vars = function
  | VarDecl x -> [get_typed_symbol x]
  | Assign (x,_) -> get_stmt2_vars x
  | _ -> []
(* FIXME: We also need to get the dimensions so that even those can be
   removed*)
and get_stmt2_vars = function
  | h::t -> 
    (match h with
      | AllTypedSymbol x -> [get_typed_symbol x]
      | _ -> [])
  | [] -> []

let rec remove_from_consts = function
  | h::t -> Hashtbl.remove consts h; remove_from_consts t
  | [] -> ()

let get_tbl node = 
  try
    Hashtbl.find nodes node
  with
    | Not_found -> raise (Internal_compiler_error "Compiler error constant propogation (get_tbl) from node not found in hash")

let list_from_hash ll tbl = 
  Hashtbl.iter (fun x y -> ll := ((x,y) :: !ll)) tbl

let rec hash_from_list hash = function
  | h::t -> Hashtbl.add hash (match h with (x,y) -> x) (match h with (x,y) -> y); hash_from_list hash t
  | [] -> ()

(* this needs to be put back into the nodes hashtbl *)
let rec rep consts = function
  |  h::t -> 
    (try
       let v = Hashtbl.find consts (match h with (x,_) -> x) in
       ((match h with (x,_) -> x),v):: rep consts t
     with 
       | Not_found -> raise (Internal_compiler_error "Compiler error constant propogation (rep tbl) from const not found in hash of node"));
  | [] -> [] 

let replace_consts_in_tbl tbl =
  let ll = ref [] in
  list_from_hash ll tbl;
  let rll = rep consts !ll in
  let hash = Hashtbl.create 20 in
  hash_from_list hash rll;
  hash

(* This is the function call that makes the tops within 
   the second iteration of the loop
*)
let rec replace_node_tables = function
  | Conditionalnode (_,x,y) -> 
    let () = replace_node_tables x in
    let () = replace_node_tables y in ()
  | Startnode (_,y) -> replace_node_tables y
  | Endnode (_,y) | Squarenode (_,y) as s -> 
    let tbl = get_tbl s in
    let rll = replace_consts_in_tbl tbl in
    Hashtbl.replace nodes s rll;
    replace_node_tables y
  | Backnode _ -> ()
  | Empty -> ()


let rec propogate_cfg = function
  | Squarenode (x,y) as s -> 
    let () = get_stmt_const x in
    (* Copy all the symbols defined until now to this node for constant folding later on*)
    Hashtbl.add nodes s (Hashtbl.copy consts);
    propogate_cfg y
  | Empty -> () (* Reached the end, basically of this graph branch *)
  (* FIXME: You are not checking that the loop bounds do not exceed the dimensions of the matrix.
  you are also not checking that strides are not manipulating the loop in such a way that it gives you
  problems --> should be done after or during constant folding
  *)
  | Startnode (_,y) -> propogate_cfg y
  | Endnode (x,y) as s -> 
    let () = propogate_cfg y in
    Hashtbl.add nodes s (Hashtbl.copy consts);
    (* These need to be removed from the hashtbl consts *)
    let dvars = get_dvars x in
    remove_from_consts dvars
  | Conditionalnode (_,y,z) -> 
    let () = propogate_cfg y in
    let () = propogate_cfg z in ()
  | Backnode x  -> 
    replace_node_tables (match !x with Conditionalnode (_,x,_) -> x | _ -> raise (Error "BackEdge not pointing to a conditional node"))

let rec is_there counter = function
  | h::t -> if counter = h then true else (is_there counter t)
  | [] -> false

let rec rem_from_dim to_rem_list counter = function
  | h::t -> (if not(is_there counter to_rem_list) then [h] else []) @ (rem_from_dim to_rem_list (counter+1) t)
  | [] -> []

let rec simple_epxr_const v = function
  | Const (x,y) -> VConst (x,y)
  | VarRef x -> 
    (try 
       Hashtbl.find v (get_symbol x)
     with | Not_found -> raise (Internal_compiler_error (("simple_epxr_const: Var " ^ (get_symbol x)) ^ " not defined before use")))
       (* Try floding expressions and see if you get a constant *)
  | AddrRef x -> Hashtbl.find v (get_addressed_symbol x)
  | Plus (x,y) ->
    let lvalue = get_simexpr_const x in
    let rvalue = get_simexpr_const y in
    process lvalue rvalue BatInt.add BatFloat.add
  | Minus (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.sub BatFloat.sub
  | Times (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.mul BatFloat.mul
  | Div (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.div BatFloat.div
  | Pow (x,y) -> process (get_simexpr_const x) (get_simexpr_const y) BatInt.pow BatFloat.pow
  | Cast (d,y) -> 
    let ret = simple_epxr_const v y in
    (match ret with | VConst (_,x) -> VConst(d,x) | _ as s -> s)
  | Brackets x | Opposite x -> simple_epxr_const v x
  | ColonExpr (x,y,z) -> raise (Internal_compiler_error ("ColonExpr detected after all rewrites!!"))
  | _ -> Top DataTypes.None

let get_dim_spec_expr v = function
  | DimSpecExpr x -> simple_epxr_const v x

let rec get_brac_dim_list v = function
  | h::t -> (get_dim_spec_expr v h) :: (get_brac_dim_list v t)
  | [] -> []

let rec get_typed_sym_dim v = function
  | SimTypedSymbol (_,x) -> []
  | ComTypedSymbol (_,x) -> 
    (try
       match x with AddressedSymbol (_,x,y) -> ((get_dimlist_1 v x) @ (get_dimlist_2 v y))
     with 
       | Not_found -> raise (Internal_compiler_error "Could not find variable get_typed_sym_dim"))
and get_dimlist_1 v = function
  | h::t -> (match h with AngleDimExpr x -> (get_dim_spec_expr v x)) ::  get_dimlist_1 v t
  | [] -> []
and get_dimlist_2 v = function
  | h::t -> (match h with BracDim x -> (get_brac_dim_list v x)) @  get_dimlist_2 v t
  | [] -> []
and get_dims list v s = 
  list := get_typed_sym_dim v s
and get_vardecl_dims list name v = function
    | h::t -> 
      (match h with 
	| (x,_) ->
	  (match x with 
	    | Squarenode (x,_) ->
	      (match x with
		| VarDecl x -> if name = (get_typed_symbol x) then get_dims list v x
		  else get_vardecl_dims list name v t
		| Assign (x,_) -> get_ass_list list name v x
		| _ -> get_vardecl_dims list name v t)
	    | _ -> get_vardecl_dims list name v t))
    | [] -> ()
and get_ass_list list name v = function
  | h::t -> 
    (match h with
      | AllTypedSymbol x -> if name = (get_typed_symbol x) then get_dims list v x else ()
      | _ -> ()); get_ass_list list name v t
  | [] -> ()

let rec get_fcall_consts fcall nodes = function
  (* Only square nodes can have the assignment statement *)
  | h::t ->
    (match h with 
      (* v is the hashtbl connected to the fcall node *)
      | (x,v) -> 
	(match x with 
	  | Squarenode (x,_) -> 
	    (match x with 
	      | Assign (x,y) -> 
		if y = fcall then 
		  let args = (match y with 
		    | FCall x -> (match  x with Call (_,y) -> y)
		    | _ -> []) in
		  ((get_fcaldim_consts nodes v args) @ (get_result_consts nodes v x))
		else get_fcall_consts fcall nodes t
	      | _ -> [])
	  | _ -> get_fcall_consts fcall nodes t))
  | [] -> []

and get_result_consts nodes v = function
  | h::t -> get_allsym_consts v nodes h :: get_result_consts nodes v t
  | [] -> []

and get_allsym_consts v nodes = function
  | AllSymbol x -> 
    (try
       let arg_val = Hashtbl.find v (get_symbol x) in
       (*dim_list can only be a list of consts, each const is the size
	 of the dimension and length of list is the number of dimensions*)
       let dim_list = ref [] in 
       get_vardecl_dims dim_list (get_symbol x) v nodes;
       let arg_list =
	 if List.length !dim_list = 0 then [] else !dim_list in (* give back all the dimensions *)
       (arg_val,arg_list) 
     with | Not_found -> raise (Internal_compiler_error "Could not find variable (get_allsym_consts)"))

  | AllAddressedSymbol x -> 
    (try
       let arg_val = Hashtbl.find v (get_addressed_symbol x) in
       let dim_list = ref [] in 
       let to_rem_list = ref [] in
       let torem = ref 0 in
       get_vardecl_dims  dim_list (get_addressed_symbol x) v nodes;
       remove_from_dim_list  torem to_rem_list (match x with AddressedSymbol (_,_,y) -> y);
       let arg_list = (rem_from_dim !to_rem_list 0 !dim_list) in
       (arg_val, arg_list)
     with | Not_found -> raise (Internal_compiler_error "Could not find variable (get_allsym_consts)"))

  (* This is fine *)
  | AllTypedSymbol x -> 
    (try
       ( (Hashtbl.find v (get_typed_symbol x)) , (get_typed_sym_dim v x))
     with | Not_found -> raise (Internal_compiler_error "Could not find variable (get_allsym_consts)"))

(* function input is the argument list *)
and get_fcaldim_consts nodes v = function
  | h::t -> 
    (try
       let arg = (match h with
	 | CallAddrressedArgument x -> get_addressed_symbol x
	 | CallSymbolArgument x -> get_symbol x) in
       let arg_val = Hashtbl.find v arg in (*Can be VConst or Top *)
       (*dim_list can only be a list of consts, each const is the size
	 of the dimension and length of list is the number of dimensions*)
       let dim_list = ref [] in 
       (match h with
	 | CallSymbolArgument x -> get_vardecl_dims  dim_list (get_symbol x) v nodes 
	 | CallAddrressedArgument x -> get_vardecl_dims  dim_list (get_addressed_symbol x) v nodes);
       (* First get the symbols used in the arguments themselves *)
       let arg_list =
	 (match h with
	   | CallSymbolArgument x -> if List.length !dim_list = 0 then [] else !dim_list (* give back all the dimensions *)
	   (* no more angle dimensions in the programs at all *)
	   | CallAddrressedArgument x -> 
	     (* Now actually remove from dim_list *)
	     (* This thing is dropping the indexed dimensions e.g., int
		M[20][30] but we have a call, FCall(M[i]) that means we
		only are taking as input a 1D Vector, since [i]
		dereferences the 1D vector.*)
	     let to_rem_list = ref [] in
	     let torem = ref 0 in
	     remove_from_dim_list  torem to_rem_list (match x with AddressedSymbol (_,_,y) -> y ) ;
	     rem_from_dim !to_rem_list 0 !dim_list
	 ) in
       (arg_val,arg_list) :: get_fcaldim_consts nodes v t;
     with
       | Not_found -> raise (Internal_compiler_error "constant propogation, could not find in hashtbl in get_fcaldim_consts"))
  | [] -> []
and remove_from_dim_list torem list = function
    | h::t ->
      (match h with
	| BracDim x -> remove_from_brac_list torem list x); 
      torem := !torem+1;
      remove_from_dim_list torem list t
    | [] -> ()
and remove_from_brac_list torem list = function
  | h::t ->
    (match h with
      | DimSpecExpr x -> if is_expr x then (list:= !torem::!list)); 
    torem := !torem + 1;
    remove_from_brac_list torem list t
  | [] -> ()
and is_expr = function
  | TStar | TStarStar -> true
  | _ -> false


(* We are not allowing higher order dependently type polymorphic lambda
   calculus. But, this needs to change if we want to add that as well
*)
let get_it = function
  | Const (x,y) -> ("NULL", VConst (x,y))
  | VarRef x -> ((get_symbol x), (Top DataTypes.Int32))
  (* Try floding expressions and see if you get a constant *)
  | _ -> raise (Error "We currently do not support higher order dependent types")

let rec resolve_dims = function
  | h::t -> 
    (match h with
      | BracDim x -> res_dims x) @ resolve_dims t
  | [] -> []
and res_dims = function
  | h::t -> 
    (match h with 
      | DimSpecExpr x -> get_it x) :: res_dims t
  | [] -> []

let rec check_and_set arg_list counter name = function
  | h::t -> 
    let va = List.nth arg_list counter in
    (match h with
      | (x,y) -> 
	if x = "NULL" then
	  if va <> y then raise (Error (name ^ "dimension constant are not equal"))
	  else ()
	else Hashtbl.add consts x va)
  | [] -> ()

let rec put_consts mconsts counter = function
  | h::t ->
    let (arg_val,arg_list) = List.nth mconsts counter in
    (match h with
      | VarDecl x -> 
	(match x with
	  | SimTypedSymbol (_,x) -> 
	    if arg_list <> [] then raise (Error (("Argument " ^ (get_symbol x)) ^ " not of type scalar"));
	    Hashtbl.add consts (get_symbol x) arg_val
	  | ComTypedSymbol (_,x) -> 
	    (match x with
	      | AddressedSymbol (x,_,y) ->
		let dim_list = resolve_dims y in
		if List.length dim_list <> List.length arg_list
		then raise (Error (("Argument " ^ (get_symbol x)) ^ " not of type scalar"));
		(* Now set the consts of dim_list has a string tuple or check that consts are equal *)
		check_and_set arg_list 0 (get_symbol x) dim_list;
		Hashtbl.add consts (get_symbol x) arg_val
	    )
	)
      | _ -> raise (Internal_compiler_error "put_consts: got something other than vardecl for interface")); 
    (put_consts mconsts (counter + 1) t)
  | [] -> ()

(* Inter-procedural constant propogation and type-checking *)
let propogate_interface fcall prev_topnode inous = 
  try
    match prev_topnode with 
      | Null -> raise Nothing
      | _ -> 
	let nodes = Hashtbl.find top_nodes prev_topnode in
	let ll1 = ref [] in
	list_from_hash ll1 nodes;
	let mconsts = get_fcall_consts fcall !ll1 !ll1 in
	(* type_check consts x *)

	(* First check that you are taking in and giving out exact number of ins/outs *)
	if List.length inous <> List.length mconsts then 
	  raise (Error "Number of input and outputs do not match")
	(* add_to_consts *)
	else put_consts mconsts 0 inous;
  with
    | Nothing -> ()
    | Not_found -> raise (Internal_compiler_error "(propogate_interface: ) topnode not found in the hashtbl")

let rec get_ins_and_outs = function
  | Squarenode (x,y) -> x::get_ins_and_outs y
  | Empty -> []
  | _ -> raise (Internal_compiler_error "get_ins_and_outs INS/OUTS are not of Squarenode type, CFG construction error")

let propogate_cfg_list prev_topnode ins outs body fcall = 
  let inous = ((get_ins_and_outs ins) @ (get_ins_and_outs outs)) in
  let () = propogate_interface fcall prev_topnode inous in
  let () = propogate_cfg body in ()

let propogate_topnode prev_topnode = function
  | Topnode (fcall, x,y) as s ->
    let () = Hashtbl.clear consts in
    let () = Hashtbl.clear nodes in
    (* y is the cfg list: inputs, outputs, and the body *)
    let () = propogate_cfg_list prev_topnode (List.nth y 0) (List.nth y 1) (List.nth y 2) fcall in 
    (* Here consts and nodes should be full *)
    let () = Hashtbl.add top_nodes s (Hashtbl.copy nodes) in s
  (* If there is nothing in the prev_topnode, the main filter then *)
  | Null -> raise (Internal_compiler_error "TopNode is Null")

let rec propogate prev_topnode = function
  | Filternode (x,y) -> 
    let prev_topnode = propogate_topnode prev_topnode x in
    propogate_rest prev_topnode y; top_nodes
and propogate_rest prev_topnode = function
  | h::t -> let _ = propogate prev_topnode h in propogate_rest prev_topnode t
  | [] -> ()
