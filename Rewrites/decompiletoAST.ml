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

let replace_star_simpleexpr const_value = function
  | TStar -> 
    let start = Const (DataTypes.Int32s, "0") in
    let stride = Const (DataTypes.Int32s, "1") in
    let end_ = Const (DataTypes.Int32s, const_value) in
    ColonExpr (start,end_,stride)
  | TStarStar -> raise (Internal_compiler_error "The compiler currently does not support **, sorry :-(")
  | _ as s -> s

(* This is the main replace * function *)
let rec replace_stars_simpleexpr declarations = function
  | Plus (x,y) -> Plus (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y)
  | Minus (x,y) -> Minus (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y)
  | Times (x,y) -> Times (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y)
  | Div (x,y) -> Div (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y)
  | Pow (x,y) -> Pow (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y)
  | Const _ | VarRef _ as s -> s
  | Brackets x -> Brackets (replace_stars_simpleexpr declarations x)
  | AddrRef x -> AddrRef (replace_stars_addr_symbol declarations x)
  | TStarStar | TStar -> raise (Internal_compiler_error "Replacing Tstar/Tstarstar hit erroneously")
  | Opposite x -> Opposite (replace_stars_simpleexpr declarations x)
  | ColonExpr (x,y,z) -> ColonExpr (replace_stars_simpleexpr declarations x, replace_stars_simpleexpr declarations y, replace_stars_simpleexpr declarations z)
  | Cast (x,y) -> Cast (x, (replace_stars_simpleexpr declarations y))

and replace_stars_dimspecexpr const_value = function
  | DimSpecExpr x ->
    let const_value = (match const_value with DimSpecExpr x -> x) in
    let const_value = (match const_value with Const (_,x) -> (string_of_int ((int_of_string x) - 1))
      | _ -> 
	let () = IFDEF DEBUG THEN print_endline (Dot.dot_simpleexpr x) ELSE () ENDIF in
	raise (Internal_compiler_error "Typed dimspec expr not of type const")) in
    DimSpecExpr (replace_star_simpleexpr const_value x)

and replace_stars_dimspecexpr_list dimspecexpr_list counter = function
  | h::t -> 
    let () = IFDEF DEBUG THEN print_endline "In declarations 3" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline (Dot.get_string_dimspec_list dimspecexpr_list) ELSE () ENDIF in
    (replace_stars_dimspecexpr (List.nth dimspecexpr_list counter) h) 
    :: (replace_stars_dimspecexpr_list dimspecexpr_list (counter + 1) t)
  | [] -> []

and replace_stars_dimspec bracdim = function
  | BracDim x -> 
    let dimspecexpr_list = (match bracdim with BracDim x -> x) in
    BracDim (replace_stars_dimspecexpr_list dimspecexpr_list 0 x)

and replace_stars_dimspeclist bdim_list counter = function
  | h::t -> 
    let () = IFDEF DEBUG THEN print_endline "In declarations 2" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline (Dot.get_string_brac_dims bdim_list) ELSE () ENDIF in
    (replace_stars_dimspec (List.nth bdim_list counter) h) :: (replace_stars_dimspeclist bdim_list (counter+1) t)
  | [] -> []

and replace_stars_addr_symbol declarations = function
  | AddressedSymbol (x,y,z) ->
    (* I need to remove the addressed symbol from the declarations list *)
    let () = IFDEF DEBUG THEN print_endline "In declarations 1" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN (List.iter (fun x -> print_endline (Dot.dot_typed_symbol x)) declarations) ELSE () ENDIF in
    let decl = List.filter (fun n -> (match n with | ComTypedSymbol (_,n) -> (match n with | AddressedSymbol (n,_,_) -> (n = x)) | _ -> false)) declarations in
    if decl = [] then raise (Internal_compiler_error ((match x with Symbol x -> x) ^ "Not defined before use!!"))
    else if not (List.length decl = 1) then raise (Internal_compiler_error ((match x with Symbol x -> x) ^ ", variable multiply defined"))
    else ();
    AddressedSymbol (x,y, (replace_stars_dimspeclist (match (List.hd decl) with 
      | ComTypedSymbol (_,n) -> (match n with | AddressedSymbol (_,_,n) -> n) 
      | _ -> raise (Internal_compiler_error "Got a non addressed symbol, even after filtering")) 0 z))

let replace_stars_allsym declarations = function
  | AllAddressedSymbol x -> AllAddressedSymbol (replace_stars_addr_symbol declarations x)
  | _ as s -> s

let replace_stars_callargument declarations = function
  | CallAddrressedArgument x -> CallAddrressedArgument (replace_stars_addr_symbol declarations x)
  | _ as s -> s

let replace_stars_filtercall declarations = function
  | Call (x,y) -> Call (x, (List.map (fun x -> replace_stars_callargument declarations x) y))

let replace_stars_expr declarations = function
  | SimExpr x -> SimExpr (replace_stars_simpleexpr declarations x)
  | FCall x -> FCall (replace_stars_filtercall declarations x)

let replace_stars declarations = function
  | Assign (x,y) -> 
    let lvalue = List.map (fun x -> replace_stars_allsym declarations x) x in
    let rvalue = replace_stars_expr declarations y in
    Assign (lvalue,rvalue)
  | _ as s -> s

let add_var_decls declarations = function
  | VarDecl x -> declarations := x :: !declarations
  | Assign (x,y) -> declarations := (List.flatten ((List.map (fun x -> (match x with AllTypedSymbol x -> [x] | _ -> []))) x)) @ !declarations
  | _ -> ()

let get_vars = function
  | VarDecl x  -> [x]
  | Assign (x,_) -> List.flatten (List.map (fun x -> (match x with AllTypedSymbol x -> [x] | _ -> [])) x)
    (* | Block x -> List.flatten (List.map (fun x -> get_vars x) x) *)
  | _ -> []

let rec get_dvars = function
  | Block x -> List.flatten (List.map (fun x -> get_vars x) x)
  | For (x,y,z) | Par (x,y,z) -> 
    let sym = (SimTypedSymbol (DataTypes.Int32s, x)) in
    sym :: (get_vars z)
  | CaseDef case -> 
    let (clause_list,other) = (match case with Case (x,y) -> (x,y)) in
    let clause_stmt_list = List.map (fun x -> (match x with Clause (_,x) -> x)) clause_list in
    let other_stmt = (match other with Otherwise x -> x) in
    (List.flatten (List.map (fun x -> get_vars x) clause_stmt_list)) @ (get_vars other_stmt)
  | _ -> []
    

let rec decompile_cfg declarations arg = function
  | Endnode (s,x,_) -> 
    (* I should remove all decs in block end node from declarations *)
    let dvars = get_dvars s in
    (* Now remove the edecs from the declarations *)
    let () = List.iter (fun r -> declarations := List.filter (fun x -> x <> r) !declarations) dvars in
    (* Now do your usual nigga.. *)
    let () = add_to_stack x in []
  | Startnode (stmt,x) -> 
    (* First determine the type of stmt this is *)
    (match stmt with
      | CaseDef _ -> startnode_case declarations arg x
      | For (_,_,_) as s -> 
	let () = IFDEF DEBUG THEN print_endline ("Building a for stmt" ^ Dot.dot_stmt s) ELSE () ENDIF in
	startnode_for declarations arg x
      | Par (_,_,_) as s -> 
	let () = IFDEF DEBUG THEN print_endline ("Building a par stmt" ^ Dot.dot_stmt s) ELSE () ENDIF in
	startnode_par declarations arg x
      | Block _ -> startnode_block declarations arg x
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
	 | Not_found -> 
	   (match x with
	     | Assign (a,y) -> (match y with | FCall _ -> raise (Internal_compiler_error "Right hand side FCall, yet new name not found in the hashtbl") | _ -> x)
	     | _ -> x)) in
    (* First we need to add the vardecl if there are any !! *)
    let () = add_var_decls declarations stmt in
    (* Now replace the stars if there are any *)
    let stmt = replace_stars !declarations stmt in
    (S stmt) :: (decompile_cfg declarations arg y)
  | Backnode _ -> []
  | Conditionalnode (x,y,z) -> 
    (* Need to find out, which one to do*)
    (match arg with
      | CASE -> conditional_case declarations arg x y z
      | LOOP -> conditional_loop declarations arg x y z
      | _ -> raise (Internal_compiler_error "Conditional_Loop input arg not CASE/LOOP"))

and decompile_loop declarations x = 
  (* Now we should always get [S;C;O]*)
  let children_list = decompile_cfg declarations LOOP x in
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


and conditional_case declarations arg expr tbranch fbranch = 
  (* First do the true branch, it should always give a stmt list *)
  let tchildren = decompile_cfg declarations arg tbranch in
  (* Check that it is always of type stmt list *)
  let tchildren = List.map (fun x -> match x with S x -> x | C _ | O _ -> raise (Internal_compiler_error "True branch of case not giving a stmt list ")) tchildren in

  (* Now get the false branch *)
  let fchildren = decompile_cfg declarations arg fbranch in
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
    
    
and conditional_loop declarations arg expr tbranch fbranch = 
  let () = IFDEF DEBUG THEN print_endline (Dot.dot_relexpr expr) ELSE () ENDIF in
  (* First do the true branch, it should always give a stmt list *)
  let tchildren = decompile_cfg declarations arg tbranch in
  (* Check that it is always of type stmt list *)
  let tchildren = List.map (fun x -> match x with S x -> x | C _ | O _ -> raise (Internal_compiler_error "True branch of case not giving a stmt list ")) tchildren in
  (* Check that there should be alteast 2 statements in this branch *)
  if not (List.length tchildren >= 2) then raise (Internal_compiler_error "While decompiling loop conditional, we did not get atleast 2 children on the tbranch");
  (* The last statement has to be a assign stmt and it becomes O type *)
  let ostmt = List.hd (List.rev tchildren) in
  (* Go through the false branch, but we really don't get anything back *)
  let fchildren = decompile_cfg declarations arg fbranch in
  (* Check that we really don't get anything back *)
  if not (fchildren = []) then raise (Internal_compiler_error "We got something back from the loop conditionals false branch!!");
  (* Now build the Clause and the Otherwise *)
  [(C (Clause (expr, (Block (List.filter (fun x -> (not (x = ostmt))) tchildren)))));(O (Otherwise ostmt))]


(* This is the function to carry out when the start node is holding a casedef type *)
and startnode_case declarations arg x = 
  (* First call build children list with Case as the arg *)
  let children_list = decompile_cfg declarations CASE x in
  (* Next make the Casedef stmt *)
  let otherwise_l = List.filter (fun x -> (match x with O _ -> true | _ -> false)) children_list in
  let otherwise = List.map (fun x -> (match x with O x -> x | _ -> raise (Internal_compiler_error "O types not extracted propoerly"))) otherwise_l in
  let clause_list_l = List.filter (fun x -> (match x with C _ -> true | _ -> false)) children_list in
  let clause_list = List.map (fun x -> (match x with C x -> x | _ -> raise (Internal_compiler_error "O types not extracted propoerly"))) clause_list_l in
  if not (List.length otherwise = 1) then raise (Internal_compiler_error "CaseDef, no otherwise specified") else ();
  if not (List.length clause_list >= 1) then raise (Internal_compiler_error "CaseDef, no clause specified") else ();
  (* Now build the Casedef and add it to the rest of the children from the previous call *)
  (S (CaseDef (Case (clause_list,(List.hd otherwise))))) :: (decompile_cfg declarations arg (Stack.pop one_place_stack))

(* This is the function to carry out when the start node is holding a casedef type *)
and startnode_for declarations arg x = 
  (* First call build children list with Case as the arg *)
  let (x,y,z) = decompile_loop declarations x in
  (* Now add it to the rest of the children *)
  (S (For (x,y,z))) :: (decompile_cfg declarations arg (Stack.pop one_place_stack))

and startnode_par declarations arg x = 
  (* First call build children list with Case as the arg *)
  let (x,y,z) = decompile_loop declarations x in
  (* Now add it to the rest of the children *)
  (S (Par (x,y,z))) :: (decompile_cfg declarations arg (Stack.pop one_place_stack))

and startnode_block declarations arg x = 
  (* First get the children list *)
  let children_list = decompile_cfg declarations BLOCK x in
  (* Now remove the S's from the List*)
  let no_s_children_list = List.map (fun x -> match x with S x -> x 
    | _ -> raise (Internal_compiler_error "While decompiling block stmt, got a non \"S\" type ")) children_list in
  (* Now pop the endnode and make the rest of the children *)
  (S (Block no_s_children_list)) :: (decompile_cfg declarations arg (Stack.pop one_place_stack))


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
    (* put inputs and outputs in the declarations list *)
    let declarations = ref (inputs@outputs) in
    let body = decompile_cfg declarations BLOCK (List.nth cfg_list 2) in
    let body_list = List.map (fun x -> (match x with S x -> x | _ -> raise (Internal_compiler_error "Got a non-block in the Topnode"))) body in
    if name = "main" || name = "Main" then
      DefMain(Filter((Symbol name), inputs, outputs, (Block body_list)))
    else 
      begin
	(* First change the name of the filter *)
	counter := !counter + 1;
	(* Put the name in the hash map *)
	let name = Symbol (name ^ (string_of_int !counter)) in
	(* Debugging adding to the hashtbl *)
	let () = IFDEF DEBUG THEN print_endline ("Adding to hashtbl: " ^ Dot.dot_stmt fcall) ELSE () ENDIF in
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
