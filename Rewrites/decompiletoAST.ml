open Language
open Language
open CFG

exception Internal_compiler_error of string

let fcall_map = Hashtbl.create 20
let counter = ref 0
let one_place_stack = Stack.create ()

(* These are the ones being passed out, actually it is a list of these things *)
type t =
  | S of stmt
  | C of caseClause
  | O of otherwise

(* These are the ones being passed in as arguments *)
type t1 =
  | LOOP
  | CASE
  | BLOCK

let add_to_stack x = 
  if not (Stack.is_empty one_place_stack) then raise (Internal_compiler_error "Pushing more than one item in the one_place_stack")
  else Stack.push x one_place_stack

let extract_loop_variable = function
  | Assign (x,y) as s -> 
    let () = IFDEF DEBUG THEN print_endline (Dot.dot_stmt s) ELSE () ENDIF in
    let lvar = List.map (fun x -> match x with AllTypedSymbol x -> (match x with SimTypedSymbol (_,x) -> x 
      | _ -> raise (Internal_compiler_error "While decompiling loop start_decl symbol not of type SimTypedSymbol"))
      | _ -> raise (Internal_compiler_error "While decompiling loop start_decl not of type allsymbol")) x in
    if not (List.length lvar = 1) then raise (Internal_compiler_error "More than one variable declaration for loop start_decl") else ();
    ((List.hd lvar),(match y with SimExpr x -> x | _ -> raise (Internal_compiler_error "Loop start_decl rvalue not of type simpleExpr")))
  | _ -> raise (Internal_compiler_error "While decompiling Loop got a non assign in the start_decl")

let extract_loop_end_expr = function
  | LessThanEqual (_,y) -> y
  | _ -> raise (Internal_compiler_error "Loop bound expression not of type <=")

let extract_loop_stride_expr = function
  | Assign (_,y) -> (match y with 
      SimExpr y -> 
	(match y with Plus (_,y) -> y | _ -> raise (Internal_compiler_error "While decompiling loop stride expr not of type Plus "))
      | _ -> raise (Internal_compiler_error " Loop stride expression not of type simple expression"))
  | _ -> raise (Internal_compiler_error "While decompiling Loop got a non assign in the start_decl")

let rec decompile_cfg arg = function
  | Endnode (_,x,_) -> let () = add_to_stack x in []
  | Startnode (stmt,x) -> 
    (* First determine the type of stmt this is *)
    (match stmt with
      | CaseDef _ -> startnode_case arg x
      | For (_,_,_) -> startnode_for arg x
      | Par (_,_,_) -> startnode_par arg x
      | Block _ -> startnode_block arg x
      | _ -> raise (Internal_compiler_error "Got an unknown type stmt in the start node"))
  | Empty -> []
  | Squarenode (x,y) -> 
    (* We might need to change this expression name for the fcall *)
    let () = IFDEF DEBUG THEN print_endline (Dot.dot_stmt x) ELSE () ENDIF in
    let stmt = 
      (try
	 let nname = Hashtbl.find fcall_map x in
	 (match x with
	   | Assign (a,y) -> 
	     (match y with
	       | FCall y -> (match y with Call (_,y) -> (Assign (a, (FCall(Call (nname,y))))))
	       | _ -> raise (Internal_compiler_error "Fcall_map assign rvalue not of type Fcall!!"))
	   | _  -> raise (Internal_compiler_error "Fcall_map hashtbl not of type Assign!!"))
       with
	 | Not_found -> x) in
    (S stmt) :: (decompile_cfg arg y)
  | Backnode _ -> []
  | Conditionalnode (x,y,z) -> 
    (* Need to find out, which one to do*)
    (match arg with
      | CASE -> conditional_case arg x y z
      | LOOP -> conditional_loop arg x y z
      | _ -> raise (Internal_compiler_error "Conditional_Loop input arg not CASE/LOOP"))

and decompile_loop x = 
  (* Now we should always get [S;C;O]*)
  let children_list = decompile_cfg LOOP x in
  if not (List.length children_list = 3) then raise (Internal_compiler_error "For loop decompilation did not get 3 children") else ();
  let start_decl = (match (List.nth children_list 0) with | S x -> x | _ -> raise (Internal_compiler_error "Loop start_decl not S stmt type")) in
  let stride_decl = (match (List.nth children_list 2) with | O x -> (match x with Otherwise x -> x) 
    | _ -> raise (Internal_compiler_error "Loop stride_dec not of O type")) in
  let (end_expr , body) = (match (List.nth children_list 1) with | C x  -> (match x with Clause (expr,body) -> (expr,body)) 
    | _ -> raise (Internal_compiler_error "Loop end_expr/body not of type C")) in
  (* Now extract the loop variable *)
  let (lvar, start_expr) = extract_loop_variable start_decl in
  let end_expr = extract_loop_end_expr end_expr in
  let stride_expr = extract_loop_stride_expr stride_decl in
  (* Now return the loop construct *)
  (lvar,ColonExpr(start_expr,end_expr,stride_expr),body)


and conditional_case arg expr tbranch fbranch = 
  (* First do the true branch, it should always give a stmt list *)
  let tchildren = decompile_cfg arg tbranch in
  (* Check that it is always of type stmt list *)
  let tchildren = List.map (fun x -> match x with S x -> x | C _ | O _ -> raise (Internal_compiler_error "True branch of case not giving a stmt list ")) tchildren in

  (* Now get the false branch *)
  let fchildren = decompile_cfg arg fbranch in
  (* 

     The children can be C or S or O type.  If the children are of C
     type then it is simply a clause and you don't need to do a thing.
     If there is a S type then it becomes an O type, if it is O type
     then just leave it as it is. We cannot have an S and O type
     together though!
     
  *)

  let other = List.filter (fun x -> match x with S x -> true | _ -> false ) fchildren in
  let ost = List.map (fun x -> (match x with S x -> x | _ -> raise (Internal_compiler_error "Got a non-otherwise in otherwise list, case conditional"))) other in
  let ret_list = if not (other = []) then [O((Otherwise (Block ost)))] else [] in
  (* Now make the clause/otherwise list *)
  let toadd = List.filter (fun x -> (match x with C _ | O _ -> true | _ -> false)) fchildren in
  (C (Clause (expr,(Block tchildren)))) :: (toadd  @ ret_list)
  
  
and conditional_loop arg expr tbranch fbranch = 
  let () = IFDEF DEBUG THEN print_endline (Dot.dot_relexpr expr) ELSE () ENDIF in
  (* First do the true branch, it should always give a stmt list *)
  let tchildren = decompile_cfg arg tbranch in
  (* Check that it is always of type stmt list *)
  let tchildren = List.map (fun x -> match x with S x -> x | C _ | O _ -> raise (Internal_compiler_error "True branch of case not giving a stmt list ")) tchildren in
  (* Check that there should be alteast 2 statements in this branch *)
  if not (List.length tchildren >= 2) then raise (Internal_compiler_error "While decompiling loop conditional, we did not get atleast 2 children on the tbranch");
  (* The last statement has to be a assign stmt and it becomes O type *)
  let ostmt = List.hd (List.rev tchildren) in
  (* Go through the false branch, but we really don't get anything back *)
  let fchildren = decompile_cfg arg fbranch in
  (* Check that we really don't get anything back *)
  if not (fchildren = []) then raise (Internal_compiler_error "We got something back from the loop conditionals false branch!!");
  (* Now build the Clause and the Otherwise *)
  [(C (Clause (expr, (Block (List.filter (fun x -> (not (x = ostmt))) tchildren)))));(O (Otherwise ostmt))]


(* This is the function to carry out when the start node is holding a casedef type *)
and startnode_case arg x = 
  (* First call build children list with Case as the arg *)
  let children_list = decompile_cfg CASE x in
  (* Next make the Casedef stmt *)
  let otherwise_l = List.filter (fun x -> (match x with O _ -> true | _ -> false)) children_list in
  let otherwise = List.map (fun x -> (match x with O x -> x | _ -> raise (Internal_compiler_error "O types not extracted propoerly"))) otherwise_l in
  let clause_list_l = List.filter (fun x -> (match x with C _ -> true | _ -> false)) children_list in
  let clause_list = List.map (fun x -> (match x with C x -> x | _ -> raise (Internal_compiler_error "O types not extracted propoerly"))) clause_list_l in
  if not (List.length otherwise = 1) then raise (Internal_compiler_error "CaseDef, no otherwise specified") else ();
  if not (List.length clause_list >= 1) then raise (Internal_compiler_error "CaseDef, no clause specified") else ();
  (* Now build the Casedef and add it to the rest of the children from the previous call *)
  (S (CaseDef (Case (clause_list,(List.hd otherwise))))) :: (decompile_cfg arg (Stack.pop one_place_stack))

(* This is the function to carry out when the start node is holding a casedef type *)
and startnode_for arg x = 
  (* First call build children list with Case as the arg *)
  let (x,y,z) = decompile_loop x in
  (* Now add it to the rest of the children *)
  (S (For (x,y,z))) :: (decompile_cfg arg (Stack.pop one_place_stack))

and startnode_par arg x = 
  (* First call build children list with Case as the arg *)
  let (x,y,z) = decompile_loop x in
  (* Now add it to the rest of the children *)
  (S (Par (x,y,z))) :: (decompile_cfg arg (Stack.pop one_place_stack))

and startnode_block arg x = 
  (* First get the children list *)
  let children_list = decompile_cfg BLOCK x in
  (* Now remove the S's from the List*)
  let no_s_children_list = List.map (fun x -> match x with S x -> x 
    | _ -> raise (Internal_compiler_error "While decompiling block stmt, got a non \"S\" type ")) children_list in
  (* Now pop the endnode and make the rest of the children *)
  (S (Block no_s_children_list)) :: (decompile_cfg arg (Stack.pop one_place_stack))


let rec decompile_filter_params = function
  | Squarenode (stmt,cfg) -> 
    (match stmt with 
      | VarDecl x -> x
      | _ -> raise (Internal_compiler_error "Inputs/Outputs not of type VarDecl!!")) :: decompile_filter_params cfg
  | Empty -> []
  | _ -> raise (Internal_compiler_error "Inputs/Outputs not declared in a square node!!")

let decompile_topnode = function
  | Topnode (fcall,name,cfg_list) -> 
    let () = IFDEF DEBUG THEN print_endline ("Filter: " ^ name) ELSE () ENDIF in
    let inputs = decompile_filter_params (List.nth cfg_list 0) in
    let outputs = decompile_filter_params (List.nth cfg_list 1) in
    let body = decompile_cfg BLOCK (List.nth cfg_list 2) in
    let body_list = List.map (fun x -> (match x with S x -> x | _ -> raise (Internal_compiler_error "Got a non-block in the Topnode"))) body in
    if name = "main" || name = "Main" then
      DefMain(Filter((Symbol name), inputs, outputs, (Block body_list)))
    else 
      begin
	(* First change the name of the filter *)
	counter := !counter + 1;
	(* Put the name in the hash map *)
	let name = Symbol (name ^ (string_of_int !counter)) in
	Hashtbl.add fcall_map fcall name;
	let body_list = List.map (fun x -> (match x with S x -> x | _ -> raise (Internal_compiler_error "Got a non-block in the Topnode"))) body in
	Def(Filter(name, inputs, outputs, (Block body_list)))
      end
  | Null -> raise (Internal_compiler_error "Got a Null type while decompiling topnode to AST")

let rec decompile_filter = function
  | Filternode (x,y) -> 
    let ll = List.map (fun x -> decompile_filter x) y in
    let main = decompile_topnode x in
    (main :: List.flatten ll)

let decompile cfg = Program (decompile_filter cfg)
