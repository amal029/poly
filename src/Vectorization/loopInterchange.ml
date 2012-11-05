open Language
open Language
open Vectorization

exception Internal_compiler_error of string

module List = Batteries.List

type loop_type =
  | PAR of symbol * simpleExpr * (int * int)

let get_symbol = function
  | Symbol (x,_) -> x

let check c = function
  | PAR (x,_,_) -> 
    let () = IFDEF DEBUG THEN print_endline ("Checking: " ^ (get_symbol x) ^ " " ^ (get_symbol c)) ELSE () ENDIF in
    not ((get_symbol x) = (get_symbol c))
  | _ -> raise (Internal_compiler_error "Got a non Colon Expr for Par simExpr")

let rec is_independent_simexpr expr = function
  | Plus (x,y,_) | Minus (x,y,_) | Div (x,y,_) | Times (x,y,_)
  | Pow (x,y,_) | Rshift (x,y,_) | Lshift(x,y,_) | Mod (x,y,_) -> 
    (is_independent_simexpr expr x) && (is_independent_simexpr expr y)
  | Abs (x,_) | Brackets (x,_) | Opposite (x,_) -> (is_independent_simexpr expr x)
  | VarRef (x,_) -> check x expr
  | AddrRef (x,_) -> check (match x with AddressedSymbol (x,_,_,_) -> x) expr
  | VecRef (_,x,_) -> check (match x with VecAddress (x,_,_,_) -> x) expr
  | Cast (_,x,_)  | Vector (_,x,_,_) -> is_independent_simexpr expr x
  | Const _ | Constvector _ -> true
  | _ -> raise (Internal_compiler_error "Got a wrong simple expression a star type!!")

let rec is_independent_relexpr x = function
  | LessThan (y,z,_) | LessThanEqual (y,z,_) | GreaterThan (y,z,_)
  | GreaterThanEqual (y,z,_) | EqualTo (y,z,_) 
    -> (is_independent_simexpr x y) && (is_independent_simexpr x z)
  | And (r1,r2,_) | Or (r1,r2,_) -> (is_independent_relexpr x r1) && (is_independent_relexpr x r2)
  | Rackets (r,_) -> is_independent_relexpr x r

let is_independent_expr x = function
  | ColonExpr (t,y,z,_) -> (is_independent_simexpr x t) && (is_independent_simexpr x y) && (is_independent_simexpr x z)
  | _ -> raise (Internal_compiler_error "Got a wrong colong expression")

let is_independent my_stack = function
  | For (s,x,_,_) | Par (s,x,_,_) -> 
    (let ll = List.map (fun y -> is_independent_expr y x) my_stack in List.fold_left (&&) true ll)
    && (let ll = List.map (check s) my_stack in List.fold_left (&&) true ll)
  | CaseDef (c,_) -> 
    let ll = (match c with | Case (ll,_,_) -> ll) in
    let rll = (List.map (fun (Clause (x,_,_)) -> x) ll) in
    let ll = List.map (fun x -> (List.fold_left (&&) true (List.map (fun y -> is_independent_relexpr y x) my_stack))) rll  in
    List.fold_left (&&) true ll
  | _ -> raise (Internal_compiler_error "Got a non For/CaseDef stmt when performing loop interchange!!")

let build_par stmt = function
  | PAR (symbol,simpleExpr,lc) -> Par (symbol,simpleExpr,Block ([stmt],lc), lc)

let build_pars stmt pars = 
  let st = ref stmt  in
  let () = List.iter (fun x -> st := build_par !st x) (List.rev pars) in 
  !st

let is_only_loops = function
  | Block (x,_) -> List.for_all (fun x -> (match x with | Par _ | For _ | CaseDef _ -> true | _ -> false)) x
  | Par _ | For _ -> true

let rec process_stmt my_stack = function
  | For (symbol,simpleExpr,stmt,lc) as s ->
    (* If there are only par, for, and case statements internally then
       process, else just build the par statments and give it back *)
    if is_only_loops stmt then
      if ((List.length my_stack = 0) || is_independent my_stack s) then 
	For (symbol, simpleExpr, (process_stmt my_stack stmt), lc)
      else For (symbol, simpleExpr, (build_pars stmt my_stack) , lc)
    else For (symbol, simpleExpr, (build_pars stmt my_stack) , lc)
    (*   else build_pars s my_stack *)
    (* else build_pars s my_stack *)
      
  | Par (symbol,simpleExpr,stmt,lc) as s ->
    (* If there are only par and for statements internally then
       process, else just build the par statments and give it back *)
    if is_only_loops stmt then
      let () = IFDEF DEBUG THEN print_endline "Testing if Par is independent" ELSE () ENDIF in
      if ((List.length my_stack = 0) || is_independent my_stack s) then
	let () = IFDEF DEBUG THEN print_endline "Par is independent and hence will be interchanged" ELSE () ENDIF in
	process_stmt ((PAR (symbol,simpleExpr,lc)) :: my_stack) stmt
      else build_pars s my_stack
    else build_pars s my_stack

  | Block (x,lc) -> Block (List.map (process_stmt my_stack) x, lc)

  | CaseDef (x,lc) as t -> 
    if ((List.length my_stack = 0) || is_independent my_stack t) then 
      let (ll,o,lc) = (match x with | Case (l,o,lc) -> (l,o,lc)) in
      let clauses = List.map (fun (Clause (r,s,lc)) -> 
	if is_only_loops s then 
	  Clause (r, (process_stmt my_stack s), lc)
	else Clause (r,(build_pars s my_stack),lc)) ll in
      let o = (match o with | Otherwise (s,lc) -> 
	if is_only_loops s then
	  Otherwise ((process_stmt my_stack s),lc)
	else Otherwise ((build_pars s my_stack),lc))  in
      CaseDef(Case(clauses,o,lc),lc)
    else build_pars t my_stack

  | _ as s -> raise (Internal_compiler_error ("Hit a non loop node when performing loop interchange: " ^ (Dot.dot_stmt s)))

let rec process_filter_stmts = function
  | Par _ as s -> 
    let () = IFDEF DEBUG THEN print_endline ("Trying to interchange: " ^ (Dot.dot_stmt s)) ELSE () ENDIF in
    process_stmt [] s
  | For (x,y,z,lc) -> For (x,y, process_filter_stmts z, lc)
  | Block (x,lc) -> Block (List.map process_filter_stmts x,lc)
  | Split (x,lc) -> Split (process_filter_stmts x, lc)
  | CaseDef (x,lc) -> CaseDef (process_case x, lc)
  | _ as s -> s
and process_case = function
  | Case (ll,o,lc) -> Case (List.map (process_clause) ll, process_otherwise o, lc)
and process_clause = function
  | Clause (r,s,lc) -> Clause (r,process_filter_stmts s, lc)
and process_otherwise = function
  | Otherwise (x,lc) -> Otherwise (process_filter_stmts x, lc)

let process_filter = function
  | Filter (x,y,z,s,sp) -> Filter (x,y,z,process_filter_stmts s,sp)

let rec interchange = function
  | FCFG.Node (e,f,r,fl) -> FCFG.Node (e,process_filter f,r,(List.map interchange fl))
