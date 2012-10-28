open Language
open Language

exception Internal_compiler_error of string

module List = Batteries.List
module Array = Batteries.Array

type t =
  | T1 of Vipr.storage
  | T2 of Vipr.reference

let get_symbol = function
  | Symbol (x,_) -> x

let get_addressed_symbol = function
  | AddressedSymbol (x,_,_,_) -> get_symbol x

let vipr_ground_type x = Vipr.Ground x

let vipr_aggregate_type (x,y) = Vipr.Aggregate (x,y)

let vipr_aggregate_dims = function
  | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with BracDim x -> 
    List.map (fun (DimSpecExpr x) -> match x with Const (_,x,_) -> int_of_string x 
      | _ -> raise (Internal_compiler_error "DimSpecExpr not of type Const!!"))x)

let vipr_typed_symbols = function
  | SimTypedSymbol (ty,symbol,_) -> Vipr.Variable (get_symbol symbol, ty)
  | ComTypedSymbol (ty,addressedsymbol,_) -> 
    let vagt_args = (vipr_aggregate_dims addressedsymbol, ty) in
    Vipr.Array (get_addressed_symbol addressedsymbol, vipr_aggregate_type vagt_args)

let vipr_proc_inputs x = vipr_typed_symbols x
let vipr_proc_outputs x = vipr_proc_inputs x

let rec vipr_process_simple_expr = function
  | Plus (x,y,_)-> Vipr.Binop (Vipr.PLUS, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Minus (x,y,_)->Vipr.Binop (Vipr.MINUS, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Times (x,y,_)->Vipr.Binop (Vipr.TIMES, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Div (x,y,_)->Vipr.Binop (Vipr.DIV, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Pow (x,y,_) ->Vipr.Binop (Vipr.POW, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Rshift (x,y,_)->Vipr.Binop (Vipr.RSHIFT,vipr_process_simple_expr x, 
				vipr_process_simple_expr y)
  | Mod (x,y,_)->Vipr.Binop (Vipr.MOD, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | Lshift (x,y,_)->Vipr.Binop (Vipr.LSHIFT,vipr_process_simple_expr x, 
				vipr_process_simple_expr y)
  | Abs (x,_) -> Vipr.Unop (Vipr.ABS, vipr_process_simple_expr x)
  | Opposite (x,_) -> Vipr.Unop (Vipr.OPP, vipr_process_simple_expr x)
  | Cast (x,y,_) -> Vipr.Cast (x,vipr_process_simple_expr y)
  | Brackets (x,_) -> Vipr.Brackets (vipr_process_simple_expr x)
  | Constvector (_,_,x,_) -> 
    let ret = List.map (fun x -> (match x with | Const (_,x,_) -> x 
      | _ -> raise (Internal_compiler_error "Constvector without consts!!"))) (Array.to_list x) in
    Vipr.RefL ret
  | VarRef _ | AddrRef _ | Const _ as s -> vipr_process_reference s
  | _ as s -> raise (Internal_compiler_error ("Unrecognized simple expr: "^(Dot.dot_simpleexpr s)))

and process_colon_expr = function
  | ColonExpr (x,y,z,_) -> (vipr_process_simple_expr x, vipr_process_simple_expr y,
			    vipr_process_simple_expr z)

and vipr_process_reference = function
  | Const (_,x,_) -> Vipr.Ref(Vipr.Constant x)
  | VarRef (x,_) -> Vipr.Ref(Vipr.VariableRef (get_symbol x))
  | AddrRef (x,_) -> Vipr.Ref(vipr_static_or_dynamic_array_ref x)

and vipr_static_or_dynamic_array_ref = function
  | AddressedSymbol (sym,_,x,_) -> 
    let ll = (match (List.hd x) with BracDim x -> 
    List.map (fun (DimSpecExpr x) -> vipr_process_simple_expr x) x) in
    if List.for_all (fun x -> (match x with | Vipr.Ref (Vipr.Constant _) -> true | _ -> false)) ll then
      let ll = List.map (fun x -> (match x with | Vipr.Ref (Vipr.Constant x) -> (int_of_string x)
	| _ -> raise (Internal_compiler_error "Hit a non constant inside constant list!!"))) ll in
      Vipr.StaticArrayRef (get_symbol sym, Vipr.StaticIndex (ll))
    else
      Vipr.DynamicArrayRef (get_symbol sym, Vipr.DynamicIndex (ll))

let rec vipr_relexpr = function
  | LessThan (x,y,_) -> Vipr.RBinop (Vipr.LT, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | LessThanEqual (x,y,_) -> Vipr.RBinop (Vipr.LEQ, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | GreaterThan (x,y,_) -> Vipr.RBinop (Vipr.GT, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | GreaterThanEqual (x,y,_) -> Vipr.RBinop (Vipr.GEQ, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | EqualTo (x,y,_) -> Vipr.RBinop (Vipr.EQEQ, vipr_process_simple_expr x, vipr_process_simple_expr y)
  | And (x,y,_) -> Vipr.And (vipr_relexpr x,vipr_relexpr y)
  | Or (x,y,_) -> Vipr.Or (vipr_relexpr x, vipr_relexpr y)
  | Rackets (x,_) -> Vipr.RBrackets (vipr_relexpr x)

let vipr_vector_to_array_ref = function
  | VecAddress (sym,_,ll,_) ->
    let ll = List.map (fun x -> vipr_process_simple_expr x) ll in
    if List.for_all (fun x -> (match x with | Vipr.Ref (Vipr.Constant _) -> true | _ -> false)) ll then
      let ll = List.map (fun x -> (match x with | Vipr.Ref (Vipr.Constant x) -> (int_of_string x)
	| _ -> raise (Internal_compiler_error "Hit a non constant inside constant list!!"))) ll in
      Vipr.StaticArrayRef (get_symbol sym, Vipr.StaticIndex (ll))
    else
      Vipr.DynamicArrayRef (get_symbol sym, Vipr.DynamicIndex (ll))

let vipr_process_call_args = function
  | CallAddrressedArgument x -> vipr_static_or_dynamic_array_ref x
  | CallSymbolArgument x -> Vipr.VariableRef (get_symbol x)

let vipr_fcall_assign_right = function
  | FCall ((Call (id,ins,_)),_) -> ((get_symbol id), List.map vipr_process_call_args ins)
  | _ -> raise (Internal_compiler_error "Should not get a (Assign) SimExpr after checking")

let vipr_simexpr_assign_right = function
  | SimExpr x -> vipr_process_simple_expr x
  | _ -> raise (Internal_compiler_error "Should not get a (Assign) FCall after checking")

let vipr_all_sym = function
  | AllTypedSymbol x -> T1 (vipr_typed_symbols x)
  | AllSymbol x -> T2 (Vipr.VariableRef (get_symbol x))
  | AllAddressedSymbol x -> T2 (vipr_static_or_dynamic_array_ref x)
  | AllVecSymbol (_,x) -> T2 (vipr_vector_to_array_ref x)

let vipr_simexpr_assign_left r l =
  let r = vipr_simexpr_assign_right r in
  let t = vipr_all_sym l in
  (match t with
    | T1 l -> Vipr.DeclareAndAssign (l,r)
    | T2 l -> Vipr.Assign (l,r))

let vipr_fcall_assign_left r l = 
  let (r,lo) = vipr_fcall_assign_right r in
  let t = List.map vipr_all_sym l in
  let l = List.map (fun t -> (match t with
    | T1 l -> raise (Internal_compiler_error "Got a storage type with function call!!")
    | T2 l -> l)) t in
  Vipr.CallFun (r,lo,l)

let rec vipr_stmt = function
  | VarDecl (x,_) -> Vipr.Declare (vipr_typed_symbols x)

  | Assign (x,e,_,_) -> 
    (match e with 
      | SimExpr _ -> 
	(if List.length x > 1 then 
	  raise (Internal_compiler_error "Currently decompialtion to VIPR does not support more than one assignment at a time!!"));
	vipr_simexpr_assign_left e (List.hd x)
      | FCall _ -> vipr_fcall_assign_left e x)

  | Noop -> Vipr.Noop

  | Block (x,_) -> Vipr.Block (List.map vipr_stmt x)

  | CaseDef ((Case (ll,(Otherwise (o,_)),_)),_) ->
    let clause_list = List.map (fun (Clause (_,s,_)) -> (vipr_stmt s)) ll in
    let r_list = List.map (fun (Clause (r,_,_)) -> (vipr_relexpr r)) ll in
    let st = ref (vipr_stmt o) in
    let () = List.iter2 (fun r x -> st := Vipr.If (r,x,!st)) (List.rev r_list) 
      (List.rev clause_list) in
    !st

  | For (sym,x,st,_) ->
    let st = vipr_stmt st in
    let index = Vipr.Variable (get_symbol sym, DataTypes.Int32s) in
    let index1 = Vipr.VariableRef (get_symbol sym) in
    let (start,ed,stride) = process_colon_expr x in
    (* Now we need to make the end into a rel-expr *)
    let ed = Vipr.RBinop (Vipr.LEQ,Vipr.Ref index1,ed) in
    (* We need to make the stride into a statement *)
    let stride = Vipr.Assign (index1, stride) in
    Vipr.For (index,start,ed,stride,st)

  | Par (sym,x,st,_) ->
    let st = vipr_stmt st in
    let index = Vipr.Variable (get_symbol sym, DataTypes.Int32s) in
    let index1 = Vipr.VariableRef (get_symbol sym) in
    let (start,ed,stride) = process_colon_expr x in
    (* Now we need to make the end into a rel-expr *)
    let ed = Vipr.RBinop (Vipr.LEQ,Vipr.Ref index1,ed) in
    (* We need to make the stride into a statement *)
    let stride = Vipr.Assign (index1, stride) in
    Vipr.Par (index,start,ed,stride,st)

  | _ as s -> raise (Internal_compiler_error ("Vipr too stupid to handle: " ^ (Dot.dot_stmt s)))

let vipr_procedure = function
  | Filter (symbol,inputs,outputs,stmt,_) -> 
    Vipr.Procedure (get_symbol symbol,
		    List.map vipr_proc_inputs inputs, List.map vipr_proc_outputs outputs, 
		    vipr_stmt stmt)

let rec process_filter = function
  | FCFG.Node (_,filter,_,fll) -> (vipr_procedure filter) :: 
    List.flatten (List.map process_filter fll)

let process node = 
  (* Now use DelareFun DeclareEntry *)
  let vipr_procedures = process_filter node in
  let main = List.filter(fun (Vipr.Procedure (x,_,_,_)) -> x = "main") vipr_procedures in
  let others = List.filter(fun (Vipr.Procedure (x,_,_,_)) -> x <> "main") vipr_procedures in
  (if List.length main <> 1 then
      raise (Internal_compiler_error ("No main filter or more than one main filter: " 
	     ^ (string_of_int (List.length main)))));
  let ll = Vipr.DeclareEntry (List.hd main) :: (List.map (fun x -> Vipr.DeclareFun x) others) in
  Vipr.Program ll
