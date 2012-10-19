open Language
open Language
open Vectorization

exception Internal_compiler_error of string

module List = Batteries.List

type loop_type =
  | PAR of symbol * simpleExpr * (int * int)

let build_par stmt = function
  | PAR (symbol,simpleExpr,lc) -> Par (symbol,simpleExpr,Block ([stmt],lc),lc)

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
  | For (s,x,_,_) -> 
    (let ll = List.map (fun y -> is_independent_expr y x) my_stack in List.fold_left (&&) true ll)
    && (let ll = List.map (check s) my_stack in List.fold_left (&&) true ll)
  | CaseDef (c,_) -> 
    let ll = (match c with | Case (ll,_,_) -> ll) in
    let rll = (List.map (fun (Clause (x,_,_)) -> x) ll) in
    let ll = List.map (fun x -> (List.fold_left (&&) true (List.map (fun y -> is_independent_relexpr y x) my_stack))) rll  in
    List.fold_left (&&) true ll
  | _ -> raise (Internal_compiler_error "Got a non For/CaseDef stmt when performing loop interchange!!")

let build_pars stmt pars = 
  let st = ref stmt  in
  let () = List.iter (fun x -> st := build_par !st x) (List.rev pars) in 
  !st

let rec process_stmt my_stack = function
  | For (symbol,simpleExpr,stmt,lc) as s ->
    if (is_independent my_stack s) then For (symbol, simpleExpr, (process_stmt my_stack stmt), lc)
    else build_pars s my_stack
    
  | Par (symbol,simpleExpr,stmt,lc) -> 
     process_stmt ((PAR (symbol,simpleExpr,lc)) :: my_stack) stmt

  | Block (x,lc) -> Block (List.map (process_stmt my_stack) x, lc)

  | Split (x,lc) -> Split (process_stmt my_stack x, lc)

  | CaseDef (x,lc) as s -> 
    let ret = (is_independent my_stack s) in
    let () = IFDEF DEBUG THEN print_endline ("CaseDef is independent of par varref: " ^ (string_of_bool ret)) ELSE () ENDIF in
    if ret then CaseDef (process_case my_stack x, lc)
    else build_pars s my_stack

  | _ as s -> build_pars s my_stack

and process_case my_stack = function
  | Case (ll,o,lc) -> Case (List.map (process_clause my_stack) ll, process_other my_stack o, lc)

and process_clause my_stack = function
  | Clause (r,stmt,lc) -> Clause (r,process_stmt my_stack stmt,lc)

and process_other my_stack = function
  | Otherwise (x,lc) -> Otherwise (process_stmt my_stack x, lc) 

let process_filter my_stack = function
  | Filter (x,y,z,s) -> Filter (x,y,z,process_stmt my_stack s)

let rec interchange = function
  | FCFG.Node (e,f,r,fl) -> FCFG.Node (e,process_filter [] f,r,(List.map interchange fl))
