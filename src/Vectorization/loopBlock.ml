open Language
open Language
open Vectorization
open Convert

exception Internal_compiler_error of string


module List = Batteries.List

let block_counter = ref 1

let is_assign_stmt = function
  | Block (x,_) -> List.for_all (fun x -> (match x with | Assign _ -> true | _ -> false)) x
  | Assign _ -> true
  | _ -> false


let rec new_sim_expr symbol fsym = function
  | Plus (x,y,lc) -> Plus ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Minus (x,y,lc) -> Minus ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Times (x,y,lc) -> Times ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Div (x,y,lc) -> Div ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Pow (x,y,lc) -> Pow ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Lshift (x,y,lc) -> Lshift ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Rshift (x,y,lc) -> Rshift ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Mod (x,y,lc) -> Mod ((new_sim_expr symbol fsym x), (new_sim_expr symbol fsym y), lc)
  | Cast (dt,x,lc) -> Cast (dt,(new_sim_expr symbol fsym x), lc)
  | Brackets (x,lc) -> Brackets ((new_sim_expr symbol fsym x),lc)
  | Opposite (x,lc) -> Opposite ((new_sim_expr symbol fsym x),lc)
  | VarRef (x,lc) as s -> 
    if (match x with | Symbol (x,_) -> x) = symbol then
      (* Replaced!! *)
      Plus (s,VarRef(Symbol(fsym,lc),lc),lc)
    else s
  | AddrRef (x,lc) -> AddrRef ((new_addressedsymbol symbol fsym x), lc)
  | _ as s -> s

and new_dspec_expr symbol fsym = function
  | DimSpecExpr x -> DimSpecExpr (new_sim_expr symbol fsym x)

and new_dimspec_expr symbol fsym = function
  | BracDim x -> BracDim (List.map (fun x -> new_dspec_expr symbol fsym x) x)

and new_addressedsymbol symbol fsym = function
  | AddressedSymbol (x,y,z,lc) -> AddressedSymbol (x,y,(List.map (new_dimspec_expr symbol fsym) 
							  z),lc)

let new_calist symbol fsym = function
  | CallAddrressedArgument x -> CallAddrressedArgument (new_addressedsymbol symbol fsym x)
  | _ as s -> s

let new_fcall symbol fsym = function
  | Call (s,calist,lc) -> Call (s,List.map (new_calist symbol fsym) calist,lc)

let new_expr symbol fsym = function
  | FCall (x,t) -> FCall ((new_fcall symbol fsym x),t)
  | SimExpr x -> SimExpr (new_sim_expr symbol fsym x)

let new_typedsymbol symbol fsym = function
  | ComTypedSymbol (dt,x,lc) -> ComTypedSymbol (dt, new_addressedsymbol symbol fsym x, lc)
  | _ as s -> s

let new_lvalue symbol fsym = function
  | AllAddressedSymbol x -> AllAddressedSymbol (new_addressedsymbol symbol fsym x)
  | AllTypedSymbol x -> AllTypedSymbol (new_typedsymbol symbol fsym x)
  | _ as s -> s

let rec new_stmt symbol fsym = function
  | Block (x,lc) -> Block ((List.map (new_stmt symbol fsym) x), lc)
  | Assign (lvalue,rvalue,lc,t) -> Assign (List.map (new_lvalue symbol fsym) lvalue,(new_expr symbol fsym rvalue),lc,t)
  | _ -> raise (Internal_compiler_error("detected non assign statement when loop blocking and peeling"))

let block_and_peel vec_length = function

  | Par (symbol,ce,stmt,lc) as s -> 
    let (s,e,st) = get_par_bounds ce in
    let size = get_access_size s e st in
    let f_stride = (vec_length * st) in
    let n = f_stride - 1 in
    let size = get_access_size s e st in
    let loop_peel_count = (mod) size vec_length in
    (* let loop_peel_size =  loop_peel_count * vec_length in *)
    let f_end = e - loop_peel_count - 1 in
    let fsym = (match symbol with | Symbol (x,_) -> x) ^ (string_of_int !block_counter) in
    block_counter := !block_counter + 1;
    let stmt = new_stmt (match symbol with Symbol (x,_) -> x) fsym stmt in
    
    (* Now build the loop block and the loop peel parts *)
    (* Replace the symbol with symbol + new_symbol everywhere *)
    let par = Par (symbol,ColonExpr(Const(DataTypes.Int32s,"0",lc),Const(DataTypes.Int32s,(string_of_int n),lc)
      ,Const(DataTypes.Int32s,(string_of_int st),lc),lc),stmt,lc) in

    (* return this for statement *)
    let c1 = For (Symbol(fsym,lc),
		  ColonExpr(Const(DataTypes.Int32s,(string_of_int s),lc),Const(DataTypes.Int32s,(string_of_int f_end),lc)
		    ,Const(DataTypes.Int32s,(string_of_int f_stride),lc),lc),par,lc) in

    (* The peeled part needs to go in as well!! *)
    let c2 = 
      if loop_peel_count > 0 then
	For (symbol,ColonExpr(Const(DataTypes.Int32s,(string_of_int (f_end+1)),lc),
			      Const(DataTypes.Int32s,(string_of_int e),lc),
			      Const(DataTypes.Int32s,(string_of_int st),lc),lc),stmt,lc)
      else Noop in
    Block ([c1;c2],lc)

  | _ -> raise (Internal_compiler_error "Got a non-par while blocking and peeling")
    

(* FIXME: 1. We need to find the datatype being accessed by the
   vector. Right now I am assuming they are 32-bit types
*)
let rec process_stmt vec_length vec_unroll_depth = function
  (* We only ever care about par loops being collapsed together!! *)

  | Par (symbol,simpleExpr,stmt,lc) as mes ->
    let (s,e,st) = get_par_bounds simpleExpr in 
    let size = get_access_size s e st in
    (* This should be size * by the datatype being accessed inside this
       loop, for now we assume it to be 32-bit type !! *)
    if (size / vec_length > vec_unroll_depth) then
      if not (is_assign_stmt stmt) then
	(For (symbol,simpleExpr,stmt,lc),None)
      else 
	(block_and_peel vec_length mes, None)
    else 
      let (x,y) = process_stmt vec_length vec_unroll_depth stmt in
      let y = (match y with Some x -> x | None -> 1) in
      if ((size*y)/vec_length) < vec_unroll_depth then
	(* Here just pass on your own Par statement *)
	(* Get the maximum from the lcl list and pass that on *)
	(mes,Some (size*y))
      else
	(* Convert this into a For loop *)
	(For(symbol,simpleExpr,stmt,lc),None)

  | Block (x,lc) -> 
    let ll = List.map (process_stmt vec_length vec_unroll_depth) x in
    let x = List.map (fun (x,_) -> x) ll in
    let y = List.map (fun (_,y) -> y) ll in
    let y = List.map (fun y -> match y with | None -> 1 | Some x -> x) y in
    let max = ref (List.hd y) in
    let () = List.iter (fun x -> if x > !max then max := x) y in
    (Block (x,lc), Some !max)

  | _ as s -> (s,None) 			(* Do nothing *)

let rec process_filter_stmts vec_length vec_unroll_depth = function
  | Par _ as s -> 
    let () = IFDEF DEBUG THEN print_endline ("Trying to block: " ^ (Dot.dot_stmt s)) ELSE () ENDIF in
    let (x,_) = process_stmt vec_length vec_unroll_depth s in x
  | For (x,y,z,lc) -> For (x,y, process_filter_stmts vec_length vec_unroll_depth z, lc)
  | Block (x,lc) -> Block (List.map (process_filter_stmts vec_length vec_unroll_depth) x,lc)
  | Split (x,lc) -> Split (process_filter_stmts vec_length vec_unroll_depth x, lc)
  | CaseDef (x,lc) -> CaseDef (process_case vec_length vec_unroll_depth x, lc)
  | _ as s -> s
and process_case vec_length vec_unroll_depth = function
  | Case (ll,o,lc) -> Case (List.map (process_clause vec_length vec_unroll_depth) ll, process_otherwise vec_length vec_unroll_depth o, lc)
and process_clause vec_length vec_unroll_depth = function
  | Clause (r,s,lc) -> Clause (r,process_filter_stmts vec_length vec_unroll_depth s, lc)
and process_otherwise vec_length vec_unroll_depth = function
  | Otherwise (x,lc) -> Otherwise (process_filter_stmts vec_length vec_unroll_depth x, lc)

let process_filter vec_length vec_unroll_depth = function
  | Filter (x,y,z,s,sp) -> Filter (x,y,z,process_filter_stmts vec_length vec_unroll_depth s,sp)

let rec block vec_length vec_unroll_depth = function
  | FCFG.Node (e,f,r,fl) -> FCFG.Node (e,process_filter vec_length vec_unroll_depth f,r,(List.map (block vec_length vec_unroll_depth) fl))
