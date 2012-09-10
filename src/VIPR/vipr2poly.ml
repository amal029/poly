open Vipr
module List = Batteries.List

exception Internal_compiler_error of string

let counter = ref 1

let get_lc = 
  counter := !counter + 1; (!counter, !counter)

let process_symbol x = Language.Language.Symbol (x,get_lc)

let process_gt_of_types = function
  | Ground x -> x
  | Aggregate (x,y) -> y
  | Tile (x,y,_) -> y

let process_dims_of = function
  | Ground _ -> raise (Internal_compiler_error ("A ground type does not have any dims"))
  | Aggregate (il,dt) -> Language.Language.BracDim (List.map (fun x -> Language.Language.DimSpecExpr (Language.Language.Const (dt, (string_of_int x), get_lc))) il)
  | Tile (il,dt,yl) -> Language.Language.BracDim (List.map (fun x -> Language.Language.DimSpecExpr (Language.Language.Const (dt, (string_of_int x), get_lc))) (il@yl))

let process_storage = function
  | Array (x,y) -> 
    Language.Language.ComTypedSymbol (process_gt_of_types y, Language.Language.AddressedSymbol (process_symbol x, [], [process_dims_of y], get_lc), get_lc)
  | Variable (x,gt) -> Language.Language.SimTypedSymbol(gt,process_symbol x,get_lc)
  | Subarray (x,y,z,i) -> raise (Internal_compiler_error "Currently subarrays are not handled in poly, because poly does not know what subarrays really are?")

let parse_const_type const = 
  let r = Str.regexp_string "[0-9]+\.[0-9]+" in 
  if Str.string_match r const 0 then Language.DataTypes.Float32 else Language.DataTypes.Int32s

let rec process_index = function
  | StaticIndex il ->
    Language.Language.BracDim (List.map (fun x -> Language.Language.DimSpecExpr (Language.Language.Const 
										   (Language.DataTypes.Int32s, (string_of_int x), get_lc))) il)
  | DynamicIndex x -> Language.Language.BracDim (List.map (fun x -> Language.Language.DimSpecExpr (process_simexpr x)) x)

and process_reference = function
  | StaticArrayRef (x,y) | DynamicArrayRef (x,y) ->
    Language.Language.AddrRef (Language.Language.AddressedSymbol (process_symbol x, [], [process_index y], get_lc), get_lc)
  | VariableRef x -> Language.Language.VarRef (process_symbol x, get_lc)
  (* Is this type always Int32s, NO - one can also pass in const as function args, which means I do not have the types for these things ??*)
  | Constant x -> Language.Language.Const (parse_const_type x, x, get_lc)

and process_simexpr = function
  | Ref x -> process_reference x
  | Binop (op, x, y) -> process_binop x y op
  | Unop (op,x) -> process_unop x op
  | Brackets x -> Language.Language.Brackets (process_simexpr x, get_lc)
  | Cast (x,y) -> Language.Language.Cast (x,process_simexpr y,get_lc)

and process_unop x = function
  | OPP -> Language.Language.Opposite (process_simexpr x, get_lc)
  | _ -> raise (Internal_compiler_error "polly does not support any unary operation other than (-)")

and process_binop x y = function
  | POW -> Language.Language.Pow (process_simexpr x, process_simexpr y, get_lc)
  | PLUS -> Language.Language.Plus (process_simexpr x, process_simexpr y, get_lc)
  | MINUS -> Language.Language.Minus (process_simexpr x, process_simexpr y, get_lc)
  | TIMES -> Language.Language.Times (process_simexpr x, process_simexpr y, get_lc)
  | DIV -> Language.Language.Div (process_simexpr x, process_simexpr y, get_lc)
  | MOD -> Language.Language.Mod (process_simexpr x, process_simexpr y, get_lc)
  | RSHIFT -> Language.Language.Rshift (process_simexpr x, process_simexpr y, get_lc)
  | LSHIFT -> Language.Language.Lshift (process_simexpr x, process_simexpr y, get_lc)
  | _ -> raise (Internal_compiler_error " Mathematical operation tried without math operators")

let r_process_op s1 s2 = function
  | EQEQ -> Language.Language.EqualTo (process_simexpr s1, process_simexpr s2, get_lc)
  | LEQ -> Language.Language.LessThanEqual (process_simexpr s1, process_simexpr s2, get_lc)
  | GEQ ->Language.Language.GreaterThanEqual (process_simexpr s1, process_simexpr s2, get_lc)
  | GT ->Language.Language.GreaterThan (process_simexpr s1, process_simexpr s2, get_lc)
  | LT ->Language.Language.LessThan (process_simexpr s1, process_simexpr s2, get_lc)
  | _ -> raise (Internal_compiler_error ("VIPR made a boo-boo, A rel-xpr without a rel-operator detected"))

let rec process_rexpr = function
  | LitTrue | LitFalse -> 
    Language.Language.EqualTo (Language.Language.Const (Language.DataTypes.Int32, "1", get_lc),
			     Language.Language.Const (Language.DataTypes.Int32, "1", get_lc), get_lc)
  | RBinop (op,s1,s2) -> r_process_op s1 s2 op
  | And (x,y) -> Language.Language.And (process_rexpr x, process_rexpr y, get_lc)
  | Or (x,y) -> Language.Language.Or (process_rexpr x, process_rexpr y, get_lc)
  | Not _ -> raise (Internal_compiler_error "poly does not support NOT expressions")


let rec process_procedure = function
  | Procedure (x,y,z,s) -> Language.Language.Filter (process_symbol x, List.map process_storage y, List.map process_storage z, process_stmt s)

and get_vec_declare = function
  | Language.Language.ComTypedSymbol (_,x,_) -> (match x with 
      | Language.Language.AddressedSymbol (x,_,h::[],lc) -> 
	let epr = (match h with Language.Language.BracDim x -> 
	  (match (List.hd x) with Language.Language.DimSpecExpr x -> x)) in
	Language.Language.AllVecSymbol ([||], (Language.Language.VecAddress (x,[],[epr],lc))),lc)
  | _ -> raise (Internal_compiler_error "Tried to convert a non aggregate type to a vector type")

(* In this function I am hoping that VIPR only contains const types!! *)
and get_const_vector lt =
  let ctyp = parse_const_type (List.hd lt) in
  Language.Language.Constvector (None,ctyp,Array.of_list (List.map (fun x -> Language.Language.Const (ctyp,x,get_lc)) lt),get_lc)

and get_loop_index = function
  | Language.Language.SimTypedSymbol (_,x,_) -> x 
  | _ -> raise (Internal_compiler_error "For index not of var type")

and get_loop_end = function
  | Language.Language.EqualTo (_,x,_) -> x
  | Language.Language.LessThan (_,x,_) -> x
  | Language.Language.LessThanEqual (_,x,_) -> x
  | Language.Language.GreaterThan (_,x,_) -> x
  | Language.Language.GreaterThanEqual (_,x,_) -> x
  | Language.Language.And (x,_,_) | Language.Language.Or (x,_,_) 
  | Language.Language.Rackets (x,_) -> get_loop_end x

and get_loop_stride = function
  | Language.Language.Assign (_,x,_) -> (match x with | Language.Language.SimExpr x -> x 
      | _ -> raise (Internal_compiler_error "Loop index cannot be incremented in a function"))
  | _ -> raise (Internal_compiler_error " Currently poly only supports assignment to loop strides")

and process_stmt = function
  | If (x,y,z) -> 
    let clause = Language.Language.Clause (process_rexpr x, process_stmt y, get_lc) in
    let otherwise = Language.Language.Otherwise (process_stmt z, get_lc) in
    let case = Language.Language.Case ([clause],otherwise,get_lc) in
    Language.Language.CaseDef (case,get_lc)
  | Declare x -> Language.Language.VarDecl (process_storage x, get_lc)
  | Assign (x,expr) -> 
    let re = process_reference x in
    let re = (match re with | Language.Language.AddrRef (x,_) -> 
      Language.Language.AllAddressedSymbol x | Language.Language.VarRef (x,_) -> Language.Language.AllSymbol x 
      | _ -> raise (Internal_compiler_error "Cannot assign to const")) in
    let se = process_simexpr expr in
    Language.Language.Assign ([re],Language.Language.SimExpr se,get_lc)
  | DeclareAndAssign (storage,expression) ->
    let re = process_storage storage in
    let ex = process_simexpr expression in
    Language.Language.Assign ([Language.Language.AllTypedSymbol re], Language.Language.SimExpr ex,get_lc)
  | DeclareArrayConst (storage,expression) -> 
    (* Make this into a vector type *)
    (* 1.) We send a block back with a.) A comtyped symbol declaration
       b.) A vector assignment to this declaration *)
    let re = process_storage storage in
    (* let rev = get_vec_declare re in *)
    let sev = get_const_vector expression in
    (* Language.Language.Block ([ Language.Language.VarDecl (re,get_lc);Language.Language.Assign([Language.Language.AllTypedSymbol re] *)
    (* 												 ,Language.Language.SimExpr sev,get_lc)],get_lc) *)
    Language.Language.Assign([Language.Language.AllTypedSymbol re],Language.Language.SimExpr sev,get_lc)
  | Block x -> Language.Language.Block ((List.map process_stmt x), get_lc)
  | Noop -> Language.Language.Noop
  | For (x,y,z,s1,s2) -> 
    let index = process_storage x in
    let index = get_loop_index index in
    let start = process_simexpr y in
    let e = process_rexpr z in
    let e = get_loop_end e in
    let str = process_stmt s1 in
    let str = get_loop_stride str in
    let body = process_stmt s2 in
    Language.Language.For (index,Language.Language.ColonExpr (start,e,str,get_lc), body,get_lc)
  | Par (x,y,z,s1,s2) ->
    let index = process_storage x in
    let index = get_loop_index index in
    let start = process_simexpr y in
    let e = process_rexpr z in
    let e = get_loop_end e in
    let str = process_stmt s1 in
    let str = get_loop_stride str in
    let body = process_stmt s2 in
    Language.Language.Par (index,Language.Language.ColonExpr (start,e,str,get_lc), body,get_lc)
  | CallFun (id,inputs,outputs) -> 
    let id = process_symbol id in
    (* Just check that none of them are things that I cannot consume *)
    let () = List.iter (fun x -> (match x with | Constant _ -> 
      raise (Internal_compiler_error " Cannot handle constants in poly fcalls") 
      | _ -> ())) inputs in
    let () = List.iter (fun x -> (match x with | Constant _ -> 
      raise (Internal_compiler_error " Cannot handle constants in poly fcalls") 
      | _ -> ())) outputs in
    let inputs = List.map process_reference inputs in
    let inputs = List.map (fun x -> (match x with | Language.Language.AddrRef (x,_) -> Language.Language.CallAddrressedArgument x 
      | Language.Language.VarRef (x,_) -> Language.Language.CallSymbolArgument x
      | _ -> raise (Internal_compiler_error " No other type but AddrRef and VarRef can be allocated in assign"))) inputs in
    let outputs = List.map process_reference outputs in
    let outputs = List.map (fun x -> (match x with | Language.Language.AddrRef (x,_) -> Language.Language.AllAddressedSymbol x 
      | Language.Language.VarRef (x,_) -> Language.Language.AllSymbol x 
      | _ -> raise (Internal_compiler_error " No other type but AddrRef and VarRef can be allocated in assign"))) outputs in
    let fc = Language.Language.Call (id,inputs,get_lc) in
    Language.Language.Assign (outputs,Language.Language.FCall (fc,false), get_lc)
  | _ -> raise (Internal_compiler_error "Filters can not be declared inside other filters!!")

let process = function
  | Program x ->
    let f = List.filter (fun x -> (match x with DeclareEntry _ | DeclareFun _ -> true | _ -> false)) x in
    let fs = List.map (fun x -> (match x with 
      | DeclareEntry x -> Language.Language.DefMain (process_procedure x, None, get_lc) 
      | DeclareFun x -> Language.Language.Def (process_procedure x, None, get_lc)
      | _ -> raise (Internal_compiler_error "Could not filter procedures out!!"))) f in
    let s = List.filter (fun x -> (match x with DeclareEntry _ | DeclareFun _ -> false | _ -> true)) x in
    if s <> [] then raise (Internal_compiler_error " Statements not inside procedure!!") else 
      Language.Language.Program fs
