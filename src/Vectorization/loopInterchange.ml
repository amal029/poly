(**

   Thu Sep  6 14:22:07 IST 2012
   
   Purpose: 
   
   This file implements loop interchange
   
   The algorithm is as follows:
   
   
   a.) Move in the loop from top to bottom
   
   For par loops:
   
   When you are returning then pop yourself from the stack and just send
   back the child as is
   
   For the for loops
   
   1.) If there is any loop inside you then just put yourself on the
   stack, else return yourself as is.  2.) When you are returning then
   check all the par loops above you if there are any par loops above
   you then you have to put those before you. Finally you put yourself
   in and pop yourself from the stack.

   One of the consequences of this algorithm is that we end up getting
   split loops. Is this a good thing or a bad thing??

*)

open Language
open Language
open Vectorization

exception Internal_compiler_error of string

module List = Batteries.List

type loop_type =
  | FOR of symbol * simpleExpr * (int * int)
  | PAR of symbol * simpleExpr * (int * int)

let rec push_loop = function
  | For _ | Par _ -> true
  | Block (x,_) -> List.fold_left (||) true (List.map push_loop x)
  | _ -> false

let get_all_pars_until_for ll = 
  List.take_while (fun x -> (match x with | PAR _ -> true | _ -> false)) ll

let build_par stmt = function
  | PAR (symbol,simpleExpr,lc) -> Par (symbol,simpleExpr,stmt,lc)
  | _ -> raise (Internal_compiler_error "Got a FOR instead of PAR!!")

let build_pars stmt pars = 
  let st = ref stmt  in
  let () = List.iter (fun x -> st := build_par !st x) pars in 
  !st

let rec process_stmt my_stack = function
  | For (symbol,simpleExpr,stmt,lc) as s ->
    let ret = 
      if push_loop stmt then 
	process_stmt ((FOR (symbol,simpleExpr,lc)) :: my_stack) stmt
      else stmt in
    (* Now we need to see how many par statements are above us and then
     interchange with all those par stmts *)
    let _ = Stack.pop in
    let pars = get_all_pars_until_for my_stack in
    let ret = build_pars stmt pars in
    For (symbol, simpleExpr, ret, lc)
    
  | Par (symbol,simpleExpr,stmt,lc) -> 
     process_stmt ((PAR (symbol,simpleExpr,lc)) :: my_stack) stmt

  | Block (x,lc) -> Block (List.map (process_stmt my_stack) x, lc)

  | Split (x,lc) -> Split (process_stmt my_stack x, lc)

  | CaseDef (x,lc) -> CaseDef (process_case my_stack x, lc)

  | _ as s -> 
    (* Get all the par stmts until a for and put them atop me*)
    let pars = get_all_pars_until_for my_stack in
    build_pars s pars

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
