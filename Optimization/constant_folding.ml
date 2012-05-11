open Language
open Language
open CFG
open Consts

exception Internal_compiler_error of string;;

let rec change_backnode cond = function
  | Conditionalnode (_,x,y) -> change_backnode cond x; change_backnode cond y
  | Squarenode (_,x) -> change_backnode cond x
  | Startnode (_,x) -> change_backnode cond x
  | Endnode (_,x) -> change_backnode cond x
  | Empty -> ()
  | Backnode x -> 
    match !x with | Empty -> x := cond | _ -> ()

let get_const consts s = 
  try
    Hashtbl.find consts s
  with
    | Not_found -> raise (Internal_compiler_error (s ^ " const_folding: not found in consts hashtbl"))

let fold_symbol consts = function
  | Symbol x -> get_const consts x

let rec fold_simple_expr consts = function
  | VarRef x as m -> 
    (match (fold_symbol consts x) with
      | VConst (x,y) -> Const (x,y)
      | _ -> m)
  | AddrRef x -> 
    let value = (match x with AddressedSymbol (x,_,_) -> fold_symbol consts x) in
    (match value with
      | VConst (x,y) -> Const (x,y)
      | _ -> AddrRef (fold_addressed_symbol consts x))
  | Brackets x -> 
    (match (fold_simple_expr consts x) with
      | Const (x,y) as t -> t
      | _ as s -> Brackets(s))
  | Cast (d,y) -> Cast(d,(fold_simple_expr consts y))
  | Opposite x -> Opposite(fold_simple_expr consts x)
  | Plus (x,y) ->
    let lvalue = fold_simple_expr consts x in
    let rvalue = fold_simple_expr consts y in 
    (match (lvalue,rvalue) with
      | (Const(x,y), Const(d,z)) -> 
	let valu = Constant_propogation.process (VConst(x,y)) (VConst(d,z)) BatInt.add BatFloat.add in
	(match valu with
	  | VConst (x,y) -> Const(x,y)
	  | _ -> Plus(lvalue,rvalue))
      | _ -> Plus(lvalue,rvalue))
    
  | Minus (x,y) -> 
    let lvalue = fold_simple_expr consts x in
    let rvalue = fold_simple_expr consts y in 
    (match (lvalue,rvalue) with
      | (Const(x,y), Const(d,z)) -> 
	(match (Constant_propogation.process (VConst(x,y)) (VConst(d,z)) BatInt.sub BatFloat.sub) with
	  | VConst (x,y) -> Const(x,y)
	  | _ -> Minus(lvalue,rvalue))
      | _ -> Minus(lvalue,rvalue))

  | Times (x,y) -> 
    let lvalue = fold_simple_expr consts x in
    let rvalue = fold_simple_expr consts y in 
    (match (lvalue,rvalue) with
      | (Const(x,y), Const(d,z)) -> 
	(match (Constant_propogation.process (VConst(x,y)) (VConst(d,z)) BatInt.mul BatFloat.mul) with
	  | VConst (x,y) -> Const(x,y)
	  | _ -> Times(lvalue,rvalue))
      | _ -> Times(lvalue,rvalue))

  | Div (x,y) -> 
    let lvalue = fold_simple_expr consts x in
    let rvalue = fold_simple_expr consts y in 
    (match (lvalue,rvalue) with
      | (Const(x,y), Const(d,z)) -> 
	(match (Constant_propogation.process (VConst(x,y)) (VConst(d,z)) BatInt.div BatFloat.div) with
	  | VConst (x,y) -> Const(x,y)
	  | _ -> Div(lvalue,rvalue))
      | _ -> Div(lvalue,rvalue))

  | Pow (x,y) -> 
    let lvalue = fold_simple_expr consts x in
    let rvalue = fold_simple_expr consts y in 
    (match (lvalue,rvalue) with
      | (Const(x,y), Const(d,z)) -> 
	(match (Constant_propogation.process (VConst(x,y)) (VConst(d,z)) BatInt.pow BatFloat.pow) with
	  | VConst (x,y) -> Const(x,y)
	  | _ -> Pow(lvalue,rvalue))
      | _ -> Pow(lvalue,rvalue))

  | Const (x,y) as s -> s
  | ColonExpr (x,y,z) -> raise (Internal_compiler_error "const_folding: Colon_expr after rewrites!!")
  | _ as s -> s

and fold_expr consts = function
  | SimExpr x -> SimExpr(fold_simple_expr consts x)
  | _ as s -> s

and fold_angle_dim_list consts = function
  | h::t -> fold_angledim_expr consts h :: fold_angle_dim_list consts t
  | [] -> []

and fold_angledim_expr consts = function
  | AngleDimExpr x -> AngleDimExpr(fold_dimspec_expr consts x)

and fold_dimspec_expr consts = function
  | DimSpecExpr x -> DimSpecExpr (fold_simple_expr consts x)

and fold_dimspec_expr_list consts = function
  | h::t -> fold_dimspec_expr consts h :: fold_dimspec_expr_list consts t
  | [] -> []

and fold_brac_dim consts = function
  | BracDim x -> BracDim(fold_dimspec_expr_list consts x)

and fold_dimspec_list consts = function
  | h::t -> fold_brac_dim consts h :: fold_dimspec_list consts t
  | [] -> []

(* If this is being called that means the addressed symbol just could
   not be folded to a constant, so we will atleast try folding the
   accesses to this addressed symbol to constants *)
and fold_addressed_symbol consts = function
  | AddressedSymbol (x,y,z) -> 
    let angle_dim_list = fold_angle_dim_list consts y in
    let dimspec_list = fold_dimspec_list consts z in
    AddressedSymbol(x,angle_dim_list,dimspec_list)

let fold_var_decl consts = function
  | ComTypedSymbol (x,y) -> 
    let s = fold_addressed_symbol consts y in
    ComTypedSymbol(x,s)
  | SimTypedSymbol (x,y) as s -> s

let rec fold_stmt consts = function
  (* If it is an addressed symbol then make the non-constant dimensions constants in here*)
  (* assignment remains NULL string*)
  | VarDecl x -> VarDecl (fold_var_decl consts x)
  | Assign (x,y) -> 
    let rvalue = fold_expr consts y in
    Assign (x,rvalue)
  | Escape x -> Escape x
  | Noop -> Noop
  | _ -> raise (Internal_compiler_error "const_folding: Unexpected statement encountered after rewrites!!")

let rec fold_cfg nodes = function
  | Startnode (stmt,x) -> Startnode (stmt, (fold_cfg nodes x))
  | Squarenode (stmt,x) as m ->
    let ret = fold_cfg nodes x in
    let s = fold_stmt (Hashtbl.find nodes m) stmt in 
    Squarenode (s,ret)
  | Conditionalnode (x,y,z) -> 
    let ret = Conditionalnode (x, (fold_cfg nodes y), (fold_cfg nodes z)) in
    let () = change_backnode ret ret in ret
  (* TODO: Check that 
     1.) If the stmt is a for/par colon expression then the expressions are constants and 
     2.) That the strides and dimensions of the colon expression are OK *)
  | Endnode (stmt,x) -> Endnode(stmt,(fold_cfg nodes x))
  | Backnode x -> Backnode (ref Empty)
  | Empty -> Empty

let rec fold_cfg_list nodes = function
  | h::t -> fold_cfg nodes h :: fold_cfg_list nodes t
  | [] -> []

let fold_topnode nodes = function
  | Topnode (f,name,y) -> Topnode(f,name,(fold_cfg_list nodes y))
  | Null -> raise (Internal_compiler_error "TopNode is null")

let rec fold top_nodes = function
  | Filternode (x,y) -> 
    let topnode = fold_topnode (Hashtbl.find top_nodes x) x in
    let ll = fold_rest top_nodes y in
    Filternode (topnode,ll)
and fold_rest top_nodes = function
  | h::t -> fold top_nodes h :: fold_rest top_nodes t
  | [] -> []
