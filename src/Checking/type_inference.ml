module Simple =
struct
  open Language
  open Language
  open Reporting

  exception Error of string;;
  exception Internal_compiler_error of string;;

  let filter_signatures = Hashtbl.create 10;;
  let curr_lnum = ref (0,0)

  let get_symbol_lc = function
    | Symbol(_,lc) -> lc
  let get_addressed_symbol_lc = function
    | AddressedSymbol(_,_,_,lc) -> lc
      
  let get_typed_symbol_lc = function
    | SimTypedSymbol (_,_,lc) -> lc
    | ComTypedSymbol (_,_,lc) -> lc
    

  let get_symbol = function
    | Symbol (x,_) -> x
  let get_addressed_symbol = function
    | AddressedSymbol (x,_,_,_) -> get_symbol x
  let get_typed_symbol = function
    | SimTypedSymbol (_,x,_) -> get_symbol x
    | ComTypedSymbol (_,x,_) -> get_addressed_symbol x

  let exists declarations s = 
    List.exists (fun x -> (get_typed_symbol x) = s ) declarations

  let get declarations s = 
    List.find (fun x -> (get_typed_symbol x) = s) declarations

  let get_typed_type = function
    | SimTypedSymbol (x,_,_) 
    | ComTypedSymbol (x,_,_) -> (match x with 
	(* | DataTypes.None -> raise (Error "Type of variable undefined, before use")  *)
	(* | DataTypes.Poly x -> raise (Error "Type of the variable polymorphic (needs to be concrete), before use")  *)
	| _ as s -> s)

  let add_to_declarations s declarations = 
    if (exists !declarations (get_typed_symbol s)) then 
      (* let () = print_curr_line_num !curr_lnum in *)
      raise (Error (((Reporting.get_line_and_column (get_typed_symbol_lc s)) ^ "Symbol "^ (get_typed_symbol s)) ^ " multiply defined" ))
    else declarations := s :: !declarations

  (*The expressions types are lists or just DataTypes.t types , because
    of -> types, but Ocaml lets one compare two lists and its elements
    with just =. This is also the reason why this function is
    polymorphic

    FIXME:
    
    1.) Currently we have exactly the same type inference algorithm like
    Ocaml.  

    2.) Do we need haskell style type-class, because currently even 2+2.0
    will not work. Everyone is forced to write the above equation like
    so: (((float32) 2)+2.0)

  *)

  let print_types l r = 
    let () = print_endline ("Left datatype : " ^ (DataTypes.print_datatype l)) in
    let () = print_endline ("Right datatype : " ^ (DataTypes.print_datatype r)) in ()

  let unify_types l r = 
    l = r
      
  let match_typed_symbol_type t = function
    | SimTypedSymbol (x,_,_) -> unify_types x t 
    | ComTypedSymbol (x,_,_) -> unify_types x t

  let ispoly = function
    | SimTypedSymbol (x,_,_) | ComTypedSymbol (x,_,_) -> match x with | DataTypes.Poly _ -> true | _ -> false

  let isnone = function
    | SimTypedSymbol (x,_,_) | ComTypedSymbol (x,_,_) -> match x with | DataTypes.None -> true | _ -> false

  let build_new_typed_symbol expr_type = function
    | SimTypedSymbol (_,x,lc) -> SimTypedSymbol(expr_type,x,lc)
    | ComTypedSymbol (_,x,lc) -> ComTypedSymbol(expr_type,x,lc)
	
  let replace_in_declaration declarations s neww = 
    let ll = List.filter (fun x -> x <> s) !declarations in
    (* Check that you are not adding the same thing more than once *)
    declarations := ll;
    let () = add_to_declarations neww declarations in  (* declarations := neww :: ll *)
    (* Now add the rest to the declarations *)
    (* let () = List.iter (fun x -> add_to_declarations x declarations) ll in *) ()

  let rec get_block_less_declarations declarations = function
    | h::t -> (List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol h)) declarations) :: get_block_less_declarations declarations t
    | [] -> []
      
  let infer_call_argument declarations = function
    | CallAddrressedArgument x -> 
      if (exists declarations (get_addressed_symbol x)) then
	let typ = (get_typed_type (get declarations (get_addressed_symbol x))) in
	let () = IFDEF DEBUG THEN print_endline (DataTypes.print_datatype typ) ELSE () ENDIF in typ
      else raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x)) ^ ("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | CallSymbolArgument x -> 
      if (exists declarations (get_symbol x)) then
	(get_typed_type (get declarations (get_symbol x)))
      else raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("Variable " ^ (get_symbol x)) ^ " is unbound"))
  let rec infer_call_argument_list declarations = function
    | h::t -> infer_call_argument declarations h :: infer_call_argument_list declarations t
    | [] -> []
      
  (*We might have to change the types on the arguments in the FCall *)
  let get_call_decs declarations = function
    | CallAddrressedArgument x -> 
      if (exists declarations (get_addressed_symbol x)) then
	(get declarations (get_addressed_symbol x))
      else raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x)) ^ ("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | CallSymbolArgument x -> 
      if (exists declarations (get_symbol x)) then
	get declarations (get_symbol x)
      else raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("Variable " ^ (get_symbol x)) ^ " is unbound"))
  let rec get_call_decs_list declarations = function
    | h::t -> get_call_decs declarations h :: get_call_decs_list declarations t
    | [] -> []
      
  
  let rec resolve_all_decs_types orig neww = function
    | SimTypedSymbol (x,y,lc) as s -> if x = orig then SimTypedSymbol (neww,y,lc) else s
    | ComTypedSymbol (x,y,lc) as s -> if x = orig then ComTypedSymbol (neww,y,lc) else s

  let infer_filtercall declarations = function
    | Call (x,y,lc) -> 
      try 
	(* let () = print_endline (get_symbol x) in *)
	(* Strip the line and column information from the symbol*)
	let (args,ret) = Hashtbl.find filter_signatures (get_symbol x) in
	let arg_types = infer_call_argument_list !declarations y in
	let call_decs = ref (get_call_decs_list !declarations y) in
	(* let () = List.iter (fun x -> print_endline ("Inputs: " ^ (DataTypes.print_datatype x)))  arg_types in *)
	(* let () = List.iter (fun x -> print_endline ("Outputs: " ^ (DataTypes.print_datatype x)))  ret in *)
      (*Now match the filter signature with the arg_types and also if
	everything unifies then just give back the output list *)
	if (List.length args) <> (List.length arg_types) then
	  raise (Error ((Reporting.get_line_and_column lc) ^ 
			   "Filter arguments and signature do not unify " ^ (get_symbol x) ^ ", size of args and input parameters not equal"))
	else 
	  (* fix this make a hashmap *)
	  (* If the args has a concrete type then they should match,
	     else if it is a poly type then the poly type needs to get
	     replaced with a concrete/poly type in a hashmap. Once this
	     is done then you need to replace the poly types in the
	     output set with the concrete types, after this replacement
	     not even a single poly type should remain in the output set
	     (ret) *)
	  let tbl = Hashtbl.create 10 in
	  let counter = ref 0 in
	  List.iter2 (fun p -> fun y -> 
	    (match (p,y) with
	      (* Case when either the parameter is None of the argument is none, should never happen *)
	      | (_,DataTypes.None) | (DataTypes.None,_)-> 
		raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify one of them is not defined yet " ^ (get_symbol x)))
	      (* Case when argument is poly and the parameter is poly, happens when a polymorphic function is applied to
	      another polymorphic function *)
	      | (DataTypes.Poly _ , DataTypes.Poly _) -> 
		(* Then the arguments become the same as parameters *)
		(* Nope, check if the hashtbl already has a binding for
		   y type, if it does then that is the new type. The
		   binding should either be concrete, if it is poly then
		   they should match, else it is an error *)
		(try 
		   let binding = Hashtbl.find tbl y in
		   (match binding with 
		     | DataTypes.Poly _ -> if (not (binding = y)) then 
			 let () =  print_types binding y in
			 raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify " ^ (get_symbol x)))
		     | _ -> () (*Case when the binding is a concrete type *));
		   (* Set the argument type to the concrete type of the parameter of the called function
		   then propogate it to all *)
		   declarations := List.map (resolve_all_decs_types p binding) !declarations;
		   call_decs := List.map (resolve_all_decs_types p binding) !call_decs;
		 with
		   | Not_found ->
		     (* Case when this is the first binding to from arguments to parameters *)
		     (* We also need to solve the constraints here *)
		     declarations := List.map (resolve_all_decs_types p y) !declarations;
		     call_decs := List.map (resolve_all_decs_types p y) !call_decs;
		     Hashtbl.add tbl y y)
	      | (_,DataTypes.Poly _) -> 
		(* Case when the argument is concrete but the parameter is polymorphic *)
		(* We also need to solve the constraints here *)
		declarations := List.map (resolve_all_decs_types y p) !declarations;
		call_decs := List.map (resolve_all_decs_types y p) !call_decs;
		(*Now replace the binding in the hashtbl if there is one *)
		Hashtbl.replace tbl y p; 
	      | (_,_) -> 
		if p <> y then
		  (match p with 
		    (* Case when the argument is polymorphic, but the argument is concrete *)
		    | DataTypes.Poly _ -> 
		      (* let neww = build_new_typed_symbol y (List.nth !call_decs !counter) in *)
		      (* let () = print_endline ((get_typed_symbol neww) ^ (DataTypes.print_datatype (get_typed_type neww))) in *)
		      (* replace_in_declaration declarations (List.nth !call_decs !counter) neww; *)
		      declarations := List.map (resolve_all_decs_types p y) !declarations;
		      call_decs := List.map (resolve_all_decs_types p y) !call_decs; 
		    | _ -> let () = print_types p y in 
			   raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify " ^ (get_symbol x)))) 
	    );
	    counter := !counter + 1;
	  ) arg_types args; (* args are the filter begin called type signature *)
	  (*Now replace all the ret*)
	  let rr = List.map (fun p ->
	    match p with
	      | DataTypes.Poly _ as s -> (try Hashtbl.find tbl s with Not_found -> 
		let () = print_types s s in
		raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify " ^ (get_symbol x))))
	      | DataTypes.None -> 
		raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify, data type output is None: " ^ (get_symbol x)))
	      | _ as s -> s) ret in
	  let () = List.iter (fun p -> match p with (* DataTypes.Poly _ *) | DataTypes.None -> 
	    let () = print_types p p in
	    raise (Error ((Reporting.get_line_and_column lc) ^ "Filter arguments and signature do not unify " ^ (get_symbol x)))
	    | _ -> ()) rr in rr
      with 
	| Not_found -> raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("Filter " ^ (get_symbol x)) ^ " is unbound"))
	  
  let rec get_used_decs declarations = function
    | VarRef (x,_) -> [get declarations (get_symbol x)]
    | AddrRef (x,_) -> [get declarations (get_addressed_symbol x)]
    | Plus (x,y,_) | Minus (x,y,_) | Times (x,y,_)
    | Div (x,y,_) | Pow (x,y,_) -> get_used_decs declarations x @ get_used_decs declarations y
    | Brackets (x,_) -> get_used_decs declarations x
    | _ -> raise (Internal_compiler_error "Simple type inference erroneously hits a a non-poly while infering math expression")

  let rec infer_simp_expr declarations = function
    | Const (x,_,_) -> x
    | Plus (x,y,lc) | Minus (x,y,lc) | Times (x,y,lc) 
    | Div (x,y,lc) | Pow (x,y,lc) | Mod(x,y,lc) | Lshift(x,y,lc) 
    | Rshift (x,y,lc) as s -> 
      let ltype = infer_simp_expr declarations x in
      let rtype = infer_simp_expr declarations y in
      if (unify_types ltype rtype) then 
	let () = (match s with 
	  | Lshift _ | Rshift _ -> 
	    (try
	       ignore (List.find (fun x -> x = ltype) DataTypes.integral);
	       ignore (List.find (fun x -> x = rtype) DataTypes.integral);
	     with
	       | Not_found -> raise (Error ((Reporting.get_line_and_column lc) ^ " lop of shift should be integral and rop should be one too!!")))
	  | _ -> ()) in ltype
      else
	(*If any one of the parts is poly then make it non poly??
	  This should not be done for None  --> FIXME
	*)
	(match (ltype,rtype) with
	  | (DataTypes.None, _) | (_,DataTypes.None) -> 
	    let () = print_types ltype rtype in
	    raise (Error ((Reporting.get_line_and_column lc) ^ "Mathematical operation branches do not unify one of them is none"))
	  | (DataTypes.Poly _, DataTypes.Poly _) ->
	    (* let rights = (get_used_decs !declarations y) in *)
	    (* List.iter (fun x -> *)
	    (*   let neww = build_new_typed_symbol ltype x in *)
	    (*   replace_in_declaration declarations x neww *)
	    (* ) rights;  *)
	    declarations := List.map (resolve_all_decs_types rtype ltype) !declarations;
	    ltype (*right becomes left *)
	  | (DataTypes.Poly _, _) -> 
	    (* let lefts = (get_used_decs !declarations x) in *)
	    declarations := List.map (resolve_all_decs_types ltype rtype) !declarations;
	    (* List.iter (fun x -> *)
	    (*   let neww = build_new_typed_symbol rtype x in *)
	    (*   replace_in_declaration declarations x neww *)
	    (* ) lefts;  *)
	    rtype (*left becomes right*)
	  | (_, DataTypes.Poly _) ->
	    declarations := List.map (resolve_all_decs_types rtype ltype) !declarations;
	    ltype (*right becomes left *)
	    (* let rights = (get_used_decs !declarations y) in *)
	    (* List.iter (fun x ->  *)
	    (*   let neww = build_new_typed_symbol ltype x in *)
	    (*   replace_in_declaration declarations x neww *)
	    (* ) rights; ltype (\*right becomes left *\) *)
	  | (_,_) -> 
	    let () = print_types ltype rtype in
	    raise (Error ((Reporting.get_line_and_column lc) ^ "Mathematical operation branches do not unify")))
    | VarRef (x,_) -> 
	if (exists !declarations (get_symbol x)) then
	  (get_typed_type (get !declarations (get_symbol x)))
	else raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("Variable " ^ (get_symbol x)) ^ " is unbound"))
    | AddrRef (x,_) -> 
	if (exists !declarations (get_addressed_symbol x)) then
	  (get_typed_type (get !declarations (get_addressed_symbol x)))
	else raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x)) ^ ("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | Brackets (x,_) -> infer_simp_expr declarations x
    | Cast (x,_,_) -> x
    | Opposite (x,_) -> infer_simp_expr declarations x
    | ColonExpr (x,y,z,_) -> 
      let f = (infer_simp_expr declarations x) in
      let s = (infer_simp_expr declarations y) in
      let t = (infer_simp_expr declarations z) in
      if (List.exists (fun x -> x = f) DataTypes.floating) || (List.exists (fun x -> x = s) DataTypes.floating) 
	|| (List.exists (fun x -> x = t) DataTypes.floating) then
	raise (Error "Colon expressions can only be of integral type")
      else if (match (f,s,t) with 
	| (DataTypes.Bool,_,_)
	| (_,DataTypes.Bool,_)
	| (_,_,DataTypes.Bool)
	| (DataTypes.None,_,_)
	| (_,DataTypes.None,_)
	| (_,_,DataTypes.None) -> true
	| (DataTypes.Poly _,_,_)
	| (_,DataTypes.Poly _,_)
	| (_,_,DataTypes.Poly _) -> true
	| _ -> false)
      then raise (Error "Colon expressions can only be of integral type")
      else f (*Just return of them*)
      (* These should all be of signed int types *)
    | TStar | TStarStar -> raise (Internal_compiler_error "Simple type inference erroneously hits a TStar/TStarStar")

  let infer_expr declarations = function
    | SimExpr x -> [infer_simp_expr declarations x]
    (* | Main -> raise (Internal_compiler_error "Simple type inference hits a Main") *)
    (* TODO 1.) Lookup the hashtbl of filter signatures a.) If filter
       signature exists then return the complete signature expression,
       because it is of type -> b.) If it does not exist then leave it
       as none it will be inferred later on. Once the signture is
       inferred later on we will need then need to traverse the ast
       again filling in the changing declarations and types again.

       c.) or just force people to write things before they are invoked
       like in Ocaml -> for now I am taking this approach*)
    | FCall (x,e) -> 
      (* let  () = print_string ("Checking FCALL ") in *)
      if not e then infer_filtercall declarations x
      else []

  let propogate_constraints lc declarations ltype rtype =
    match (ltype,rtype) with
      | (DataTypes.None, _) | (_,DataTypes.None) -> 
	let () = print_types ltype rtype in
	raise (Error ((Reporting.get_line_and_column lc) ^ "Mathematical operation branches do not unify one of them is none"))
      | (DataTypes.Poly _, DataTypes.Poly _) ->
	declarations := List.map (resolve_all_decs_types rtype ltype) !declarations;
	ltype (*right becomes left *)
      | (DataTypes.Poly _, _) -> 
	declarations := List.map (resolve_all_decs_types ltype rtype) !declarations;
	rtype (*left becomes right*)
      | (_, DataTypes.Poly _) ->
	declarations := List.map (resolve_all_decs_types rtype ltype) !declarations;
	ltype (*right becomes left *)
      | (_,_) -> 
	let () = print_types ltype rtype in
	raise (Error ((Reporting.get_line_and_column lc) ^ "Mathematical operation branches do not unify"))
	  
  let rec infer_relexpr declarations = function
    | LessThan (x,y,lc) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else
	let _ = propogate_constraints lc declarations lexpr_type rexpr_type in ()
    | LessThanEqual (x,y,lc) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let _ = propogate_constraints lc declarations lexpr_type rexpr_type in ()
    | GreaterThan (x,y,lc) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let _ = propogate_constraints lc declarations lexpr_type rexpr_type in ()
    | GreaterThanEqual (x,y,lc) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let _ = propogate_constraints lc declarations lexpr_type rexpr_type in ()
    | EqualTo (x,y,lc) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let _ = propogate_constraints lc declarations lexpr_type rexpr_type in ()
    | And (x,y,lc) -> 
      let () = (infer_relexpr declarations x) in
      let () = (infer_relexpr declarations y) in ()
    | Or (x,y,lc) -> 
      let () = (infer_relexpr declarations x) in
      let () = (infer_relexpr declarations y) in ()
    | Rackets (x,lc) ->  
      let () = (infer_relexpr declarations x) in ()
      

  let get_new_typed_symbol declarations v = function
    | SimTypedSymbol (x,_,lc)
    | ComTypedSymbol (x,_,lc) as ret -> 
      (match x with
	| DataTypes.Poly _ -> 
	  (try 
	     let w = (List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol v)) declarations) in
	     (match w with 
	       | SimTypedSymbol(DataTypes.None,_,lc) 
	       | ComTypedSymbol(DataTypes.None,_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ "Second pass, typed symbol of type none cannot be set!!"))
	       | SimTypedSymbol(_,_,lc)
	       | ComTypedSymbol(_,_,lc) as s -> s)
	   with
	     | Not_found -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ 
							       ("Simple type inference (second pass, cannot find " ^ (get_typed_symbol v)) ^ " in declarations")))
	| DataTypes.None -> raise (Error ((Reporting.get_line_and_column lc) ^ "Second pass, typed symbol of type none, should never happen in an assignment !!"))
	| _ -> ret)

  (* You have to do this for Assign with var decls as well *)
  let rec replace_var_decls declarations = function
    | Assign (x,r,lc) ->
      Assign ((List.map (fun x -> (match x with 
	| AllTypedSymbol x -> AllTypedSymbol (get_new_typed_symbol declarations x x) 
	| _ as s -> s)) x), r,lc)
    | VarDecl (x,lc) as ret ->
      let v = x in
      (match x with
	| SimTypedSymbol (x,_,_) 
	| ComTypedSymbol (x,_,_) -> 
	  (match x with
	    | DataTypes.None -> 
	      (try 
		 let w = (List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol v)) declarations) in
		 (match w with 
		   | SimTypedSymbol(DataTypes.None,_,lc) 
		   | ComTypedSymbol(DataTypes.None,_,lc) -> 
		     let () = print_endline ((Reporting.get_line_and_column lc) ^ ("Warning: Symbol " ^ (get_typed_symbol w)) ^ " not being used") in Noop
		   | SimTypedSymbol(_,_,lc)
		   | ComTypedSymbol(_,_,lc) as s -> VarDecl (s, lc)
		 )
	       with
		 | Not_found -> raise (Internal_compiler_error (("Simple type inference (second pass, cannot find " ^ (get_typed_symbol v)) ^ " in declarations")))
	    | _ -> ret))
    | Block (x,lc) -> Block (replace_block_stmts declarations x, lc)
    | For (x,y,z,lc) -> For(x,y, (replace_var_decls declarations z), lc)
    | Par(x,y,z,lc) -> Par(x,y, (replace_var_decls declarations z), lc)
    | CaseDef (x,lc) -> CaseDef (replace_casedef declarations x, lc)
    | Split (x,lc) -> Split (replace_var_decls declarations x, lc)
    | _ as s -> s
  and replace_block_stmts declarations = function
    | h::t -> replace_var_decls declarations h :: replace_block_stmts declarations t
    | [] -> []
  and replace_casedef declarations = function
    | Case (x,y,lc) -> Case ((replace_clauselist declarations x), (replace_otherwise declarations y),lc)
  and replace_clauselist declarations = function
    | h::t -> replace_clause declarations h :: replace_clauselist declarations t
    | [] -> []
  and replace_clause declarations = function
    | Clause (x,s,lc) -> 
      let r = replace_var_decls declarations s in
      let rr = (match r with | VarDecl (x,lc) -> Noop | _ as s -> s) in
      Clause (x,rr,lc)
  and replace_otherwise declarations = function
    | Otherwise (s,lc) -> 
      let r = replace_var_decls declarations s in
      let rr = (match r with | VarDecl (x,lc) -> Noop | _ as s -> s) in
      Otherwise ((replace_var_decls declarations rr),lc)

  let rec infer_stmt declarations = function
    | VarDecl (x,lc) as s -> 
      (*let () = set_curr_line_num s in*)
      let () = add_to_declarations x declarations in s
    | Assign (x,y,lc) as s -> 
      let extern = (match y with
	| FCall (_,e) -> e
	| _ -> false) in
      if extern then s
      else
	let expr_type = infer_expr declarations y in
	if List.length x <> List.length expr_type then 
	  raise (Error ((Reporting.get_line_and_column lc) ^ "Simple: Input and output types do not unify"))
	else 
	  Assign ( (infer_assign_lvalue_list declarations expr_type 0 x) , y, lc)
    | Block (x,lc) -> 
      (* let () = print_endline ("Checking block") in *)
      let vcopy = !declarations in 
      let ll = infer_stmt_list declarations x in
      let rett_stmts = replace_var_decls !declarations (Block (ll, lc)) in 
      declarations := (get_block_less_declarations !declarations vcopy);
      rett_stmts
    (* We need to replace the var decls with the proper types in here *)
    | Split (x,y) -> Split ((infer_stmt declarations x), y)
    | CaseDef (x,lc) -> CaseDef (infer_casedef declarations x, lc)
    | Par (x,y,z,lc) -> 
      let _ = infer_simp_expr declarations y in (* we don't bother with colon expr return types *)
      (*We need to declare the index variable in the declarations and
	then remove it once we are done*)
      let vcopy = !declarations in
      declarations := SimTypedSymbol(DataTypes.Int32s, x, lc)  :: !declarations;
      let ss = (match z with | VarDecl _ -> Noop | _ -> infer_stmt declarations z) in
      declarations := (get_block_less_declarations !declarations vcopy); (*Be carefule, something might have changed inside *)
      Par(x,y,ss,lc)
    | For (x,y,z,lc) -> 
      let _ = infer_simp_expr declarations y in
      (*We need to declare the index variable in the declarations and
	then remove it once we are done*)
      let vcopy = !declarations in
      declarations := SimTypedSymbol(DataTypes.Int32s, x, lc)  :: !declarations;
      let ss = (match z with | VarDecl _ -> Noop | _ -> infer_stmt declarations z) in
      declarations := (get_block_less_declarations !declarations vcopy); (*Be carefule, something might have changed inside *)
      For(x,y,ss,lc)
    | _ as s -> s
  and infer_casedef declarations = function
    | Case (x,y,lc) -> Case ((infer_casecluase_list declarations x), (infer_otherwise declarations y),lc)
  and infer_casecluase_list  declarations = function
    | h::t -> infer_caseclause declarations h :: infer_casecluase_list declarations t
    | [] -> []
  and infer_caseclause declarations = function
    | Clause (expr,stmt,lc) -> 
      let () = (infer_relexpr declarations expr) in
      (* The above inference can have the side-affect of changing the declarations list *)
      let ss = (match stmt with | VarDecl (x,lc) -> Noop | _ -> (infer_stmt declarations stmt)) in
      Clause (expr, ss,lc)
  and infer_otherwise declarations = function
    | Otherwise (x,lc) -> 
      let ss = (match x with | VarDecl (x,lc) -> Noop | _ -> (infer_stmt declarations x)) in
      Otherwise (ss,lc)
  and infer_stmt_list declarations = function
    | h::t -> 
      (* let () = print_endline ("checking stmt list") in *)
      infer_stmt declarations h :: infer_stmt_list declarations t 
    | [] -> []
  and infer_assign_lvalue_list declarations expr_type counter = function
    | h::t -> 
      (* let () = print_endline ("Checking assignment statements ") in *)
      infer_assign_lvalue declarations (List.nth expr_type counter) h :: infer_assign_lvalue_list declarations expr_type (counter+1) t
    | [] -> []
  (* It is possible to propogate the type from the left to the right side, i.e., (get_typed_type x) => expr_type *)
  and infer_assign_lvalue declarations expr_type = function
    (* Return back vardeclared or non-var declared symbols *)
    | AllTypedSymbol x as s -> 
      if (match_typed_symbol_type expr_type x) then
	(add_to_declarations x declarations; s)
      else 
	if (isnone x) then
	  let neww = build_new_typed_symbol expr_type x in
	  replace_in_declaration declarations x neww;
	  (* Debugging *)
	  let () = IFDEF DEBUG THEN List.iter (fun x -> print_endline ("Added to decl : " ^ (get_typed_symbol x))) !declarations ELSE() ENDIF in
	  AllTypedSymbol (neww)
	else
	  begin
	    if (not (ispoly x)) then
	      let () = (match expr_type with 
		| DataTypes.Poly _ -> 
		  (* Propogate a concrete type from lvalue to the rvalue *)
		  declarations := List.map (resolve_all_decs_types expr_type (get_typed_type x)) !declarations;
		| _ -> let () = print_types (get_typed_type x) expr_type in
		       raise (Error ((Reporting.get_line_and_column (get_typed_symbol_lc x)) ^ 
					("types do not unify for assignment to " ^ (get_typed_symbol x))))) in 
	      (* add the declaration to the declarations list *)
	      let () = add_to_declarations x declarations in s
	    else
	      begin
		if (ispoly x) then
		  (declarations := List.map (resolve_all_decs_types (get_typed_type x) expr_type) !declarations; s)
		else
		  let () = print_types (get_typed_type x) expr_type in
		  raise (Error ((Reporting.get_line_and_column (get_typed_symbol_lc x)) ^ ("types do not unify for assignment to " ^ (get_typed_symbol x))))
	      end
	  end
    | AllSymbol x as s ->
      (* let () = print_endline (get_symbol x) in  *)
      if (exists !declarations (get_symbol x)) then 
	if (match_typed_symbol_type expr_type (get !declarations (get_symbol x))) then s
	else 
	  if (isnone (get !declarations (get_symbol x))) then
	    let neww = build_new_typed_symbol expr_type (get !declarations (get_symbol x)) in 
	    replace_in_declaration declarations (get !declarations (get_symbol x)) neww;
	    s (* AllTypedSymbol (neww) *)
	  else 
	    (* This assures single assignment to the output types *)
	    begin
	      if (not (ispoly (get !declarations (get_symbol x)))) then
		let () = (match expr_type with 
		  | DataTypes.Poly _ -> 
		    (* Propogate a concrete type from lvalue to the rvalue *)
		    declarations := List.map (resolve_all_decs_types expr_type (get_typed_type (get !declarations (get_symbol x)))) !declarations;
		  | _ -> let () = print_types (get_typed_type (get !declarations (get_symbol x))) expr_type in
			 raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("types do not unify for assignment to " ^ (get_symbol x))))) in s
	      else
		begin
		  if (ispoly (get !declarations (get_symbol x))) then
		    (declarations := List.map (resolve_all_decs_types  (get_typed_type (get !declarations (get_symbol x))) expr_type) !declarations; s)
		  else 
		    let () = print_types (get_typed_type (get !declarations (get_symbol x))) expr_type in
		    raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("types do not unify for assignment to " ^ (get_symbol x))))
		end
	    end 
      else raise (Error ((Reporting.get_line_and_column (get_symbol_lc x)) ^ ("Variable " ^ (get_symbol x)) ^ " is unbound"))
    (* let ret = SimTypedSymbol (expr_type, x) in  *)
    (* add_to_declarations ret declarations; AllTypedSymbol(ret) *)
    | AllAddressedSymbol x as s -> 
      (* let () = print_endline (get_addressed_symbol x) in  *)
      if (exists !declarations (get_addressed_symbol x)) then 
	if (match_typed_symbol_type expr_type (get !declarations (get_addressed_symbol x))) then s
	else 
	  if (isnone (get !declarations (get_addressed_symbol x))) then
	    let neww = build_new_typed_symbol expr_type (get !declarations (get_addressed_symbol x)) in 
	    (* let () = print_endline (("Turning None into a real type " ^ (get_typed_symbol neww)) ^ (DataTypes.print_datatype (get_typed_type neww))) in *)
	    replace_in_declaration declarations (get !declarations (get_addressed_symbol x)) neww;
	    s (* AllTypedSymbol (neww) *)
	  else
	    begin
	      if (not (ispoly (get !declarations (get_addressed_symbol x)))) then
		let () = (match expr_type with 
		  | DataTypes.Poly _ -> 
		    (* Propogate a concrete type from lvalue to the rvalue *)
		    declarations := List.map (resolve_all_decs_types expr_type (get_typed_type (get !declarations (get_addressed_symbol x)))) !declarations;
		  | _ -> let () = print_types (get_typed_type (get !declarations (get_addressed_symbol x))) expr_type in
			 raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x))^ 
					  ("types do not unify for assignment to " ^ (get_addressed_symbol x))))) in s
	      else
		begin
		  if (ispoly (get !declarations (get_addressed_symbol x))) then
		    (declarations := List.map (resolve_all_decs_types  (get_typed_type (get !declarations (get_addressed_symbol x))) expr_type) !declarations; s)
		  else 
		    let () = print_types (get_typed_type (get !declarations (get_addressed_symbol x))) expr_type in
		    raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x)) ^ 
				     ("Types do not unify for assignment to " ^ (get_addressed_symbol x))))
		end
	    end
      else raise (Error ((Reporting.get_line_and_column (get_addressed_symbol_lc x)) ^ ("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
  (* else (\* This means this is a declaration lets add it to the declarations list *\) *)
  (* 	let ret = ComTypedSymbol (expr_type, x) in  *)
  (* 	add_to_declarations ret declarations; AllTypedSymbol (ret) *)

  (* This is called in the second pass, gets rid of all the variables,
     which are not used *)
  (* Making the vardecls for Addressed dimensions *)
  let get_dimspec = function
    | DimSpecExpr x -> 
      (match x with
	| VarRef (x,lc) -> [(SimTypedSymbol (DataTypes.Int32s,x,lc))]
	| Const _ -> []
	| _ -> raise (Error ("Dimensions of addressed symbols in function parameters cannot be anything but names of consts")))
  let rec get_dimspec_list = function
    | h::t -> get_dimspec h @ get_dimspec_list t
    | [] -> []
  let get_brac_dims_2 = function
    | BracDim x -> get_dimspec_list x
  let rec get_brac_dim_list = function
    | h::t -> get_brac_dims_2 h @ get_brac_dim_list t
    | [] -> []
  let get_dims_2 = function
    | AddressedSymbol (_,_,x,lc) -> get_brac_dim_list x
  let get_dim_param = function
    | SimTypedSymbol (_,_,_) -> []
    | ComTypedSymbol (_,y,_) as s -> 
      let () = print_endline ("getting dims from: " ^ (get_typed_symbol s)) in
      get_dims_2 y
  let rec get_dim_params = function
    | h::t -> 
      (* let () = print_endline "Checking" in *)
      get_dim_param h @ get_dim_params t
    | [] -> []

  let infer_filter = function
    | Filter (x,y,z,w) ->
      let () = print_string (("Filter: " ^ (get_symbol x)) ^ " : ") in
      (* If the declarations are Addr type then the dims are also vars that can be accessed *)
      let dim_decs = get_dim_params (y@z) in
      let declarations = ref ((y @ z) @ dim_decs) in
      let ret_stmt = infer_stmt declarations w in
      (* First get the outputs from the declarations and put those in this filter*)
      (* let () = List.iter (fun x -> print_endline ("decs : "^ (get_typed_symbol x))) !declarations in *)
      try 
	let zo = List.map (fun y -> List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol y)) !declarations) z in
	let zi = List.map (fun y -> List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol y)) !declarations) y in
	if List.length y = 0 then print_string ("() -> ")
	else List.iter (fun x -> print_string (("" ^ (DataTypes.print_datatype (get_typed_type x))) ^ " -> " )) zi;
	if List.length zo = 0 then print_string ("()\n")
	else 
	  (
	    let () = print_string "(" in
	    let () = List.iter (fun x -> print_string (("" ^ (DataTypes.print_datatype (get_typed_type x))) ^ " * " )) zo in
	    let () = print_endline "())" in ());
	Hashtbl.add filter_signatures (get_symbol x) ((List.map (fun x -> (get_typed_type x)) zi), (List.map (fun x -> (get_typed_type x)) zo));
	let f = Filter(x,zi,zo,ret_stmt) in f
      with
	| Not_found -> raise (Internal_compiler_error "Filter re-build declarations don't have all the outputs")

  let infer_toplevelstmt = function
    | Def (x,y,lc) -> Def (infer_filter x, y, lc)
    | DefMain (x,y,lc) -> DefMain(infer_filter x, y, lc)
    | _ as s -> s

  let rec infer_toplevelstmt_list = function
    | h::t -> infer_toplevelstmt h :: infer_toplevelstmt_list t
    | [] -> []

  let infer_ast = function
    | Program x -> Program (infer_toplevelstmt_list x)

end

(* 
   
   Types are kept as typed symbols, they are extracted and given back as
   tuples (Datatypes.t, [int; int; ....]) for unification.
   
*)

module First_order =

struct
  open Language
  open Language
  open CFG

  exception Internal_compiler_error of string;;
  exception Error of string;;
  
  let fcall_map = Hashtbl.create 20
  
  (* This function only works on declarations and hence, if the
     declarations is a SimTypedSymbol then that means it is a scalar.
     If it is a ComTypedSymbol then we need to give all the dimensions
     in the format mentioned above.  *)

  let rec print_dims = function
    | h::t -> 
      let () = print_string h in
      if not (t = []) then let () = print_string "," in print_dims t
    | [] -> ()

  let print_types (lx,ly) (rx , ry) = 
    let () = print_string (("Left datatype : " ^ (DataTypes.print_datatype lx)) ^ "[") in
    let () = (print_dims ly) in
    let () = print_endline "]" in
    let () = print_string (("Right datatype : " ^ (DataTypes.print_datatype rx)) ^ "[") in
    let () = (print_dims ry) in
    let () = print_endline "]" in ()

  let get_brac_dims = function
    | DimSpecExpr x ->
      (match x with
	| Const (x,y,_) ->
	  if (not (List.exists (fun r -> x = r) DataTypes.integral)) || ((int_of_string y) < 0) then
	    let () = IFDEF DEBUG THEN print_endline ("Dim is: " ^ y) ELSE () ENDIF in
	    raise (Error ("First_order_type_inference: Dimensions of array not of type unsignedIntegral, it is: " ^ y))
	  else y
	| _ -> raise (Internal_compiler_error "First_order_type_inference hits non-const inside dimensions after constant folding!!"))

  let get_dims = function
    | BracDim x -> List.map get_brac_dims x

  let get_first_order_type = function
    | SimTypedSymbol (x,_,_) -> (x,[])
    (* Remember we have gotten rid of all the angledim <> lists, so that
       we inly have [] and [,,,][] expressions with drop semnatics
       left *)
    | ComTypedSymbol (x,y,_) -> 
      (* let () = IFDEF DEBUG THEN print_endline "get_first_order_type: got complex typed symbol" ELSE () ENDIF in *)
      (x, List.flatten (List.map get_dims (match y with AddressedSymbol (_,_,y,_) -> y)))
      
  let drop_dimspec_expr counter_list counter = function
    | DimSpecExpr x ->
      (match x with
	| TStarStar | TStar -> ()
	| _ -> counter_list := counter :: !counter_list)

  let rec drop_dimspec_expr_list counter_list counter = function
    | h::t -> drop_dimspec_expr counter_list counter h; drop_dimspec_expr_list counter_list (counter+1) t
    | [] -> ()

  let to_drop_bracdim counter_list counter = function
    | BracDim x -> drop_dimspec_expr_list counter_list counter x

  let rec to_drop_dims counter_list counter = function
    | h::t -> 
      let () = to_drop_bracdim counter_list counter h in
      let () = IFDEF DEBUG THEN print_string "To drop" ELSE () ENDIF in
      let () = IFDEF DEBUG THEN List.iter (fun x -> print_endline ("," ^ (string_of_int x))) !counter_list ELSE () ENDIF in
      to_drop_dims counter_list (counter+1) t
    | [] -> ()

  let get_to_drop = function
    | AddressedSymbol (_,_,x,_) -> let p = ref [] in to_drop_dims p 0 x; !p
      
  let rec drop todrop counter dims = function
    | h::t -> if not (List.exists (fun x -> x = counter) todrop) then dims := h :: !dims; drop todrop (counter+1) dims t
    | [] -> ()

  let rec infer_simp_expr declarations = function
    | Const (x,_,_) -> (x,[])
    | Plus(x,y,lc) | Minus(x,y,lc) | Times (x,y,lc) | Div (x,y,lc)
    | Pow(x,y,lc) | Mod(x,y,lc) | Lshift(x,y,lc) | Rshift(x,y,lc) as s ->
      let lv = infer_simp_expr declarations x in
      let rv = infer_simp_expr declarations y in
      if (Simple.unify_types lv rv) then
	let () = (match s with 
	  | Lshift _ | Rshift _ -> 
	    (try
	       ignore (List.find (fun x -> x = (match lv with (a,_) -> a)) DataTypes.integral);
	       ignore (List.find (fun x -> x = (match rv with (a,_) -> a)) DataTypes.integral);
	     with
	       | Not_found -> raise (Error ((Reporting.get_line_and_column lc) ^ " lop of shift should be integral and rop should be one too!!")))
	  | _ -> ()) in lv
      else
	let () = print_types lv rv in
	raise (Error ((Reporting.get_line_and_column lc) ^ "First order dimensions do not unify in the mathematical operation"))
    | VarRef (x,lc) ->
      if (Simple.exists declarations (Simple.get_symbol x)) then
	(get_first_order_type (Simple.get declarations (Simple.get_symbol x)))
      else raise (Error ((Reporting.get_line_and_column lc) ^ ("Variable " ^ (Simple.get_symbol x)) ^ " is unbound"))
    | AddrRef (x,lc) ->
      if (Simple.exists declarations (Simple.get_addressed_symbol x)) then
	let dec_type = (get_first_order_type (Simple.get declarations (Simple.get_addressed_symbol x))) in
	let to_drop = (get_to_drop x) in
	if ((List.length to_drop) > (List.length (snd (dec_type)))) then
	  raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "First_order_type_inference: dropping more dimensions then the size of the polytope"))
	else
	  let actual_dims = ref [] in
	  (drop to_drop 0 actual_dims (snd dec_type));
	  (fst (dec_type)), !actual_dims
      else raise (Error ( (Reporting.get_line_and_column lc) ^ ("Variable " ^ (Simple.get_addressed_symbol x)) ^ " is unbound"))
    | Brackets (x,lc) -> infer_simp_expr declarations x
    (*Be careful here: it is not the same as Simple case, because we
      need to get the current type and dimensions of the simple
      expression and then convert the cast to this cast *)
    | Cast (x,y,lc) ->
      let dims = snd (infer_simp_expr declarations y) in
      (x,dims)
    | Opposite (x,lc) -> infer_simp_expr declarations x
    | ColonExpr (x,y,z,lc) ->
      let f = (infer_simp_expr declarations x) in
      let s = (infer_simp_expr declarations y) in
      let t = (infer_simp_expr declarations z) in
      if ((not (Simple.unify_types (snd (f)) [])) || (not (Simple.unify_types (snd (s)) [])) || (not (Simple.unify_types (snd (t)) []))) then
	raise (Error ((Reporting.get_line_and_column lc) ^ "First_order_type error: Expressions in colon expressions can only be of sclar types"))
      else f
    | TStar | TStarStar -> raise (Internal_compiler_error "First_order_type erroneously reached TStar/TStarStar while doing type inference")

  let rec infer_relexpr declarations = function
    | LessThan (x,y,lc) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error ((Reporting.get_line_and_column lc) ^ "Types do not unify in conditional expression"))
    | LessThanEqual (x,y,lc) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error ((Reporting.get_line_and_column lc) ^ "Types do not unify in conditional expression"))
    | GreaterThan (x,y,lc) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error ((Reporting.get_line_and_column lc) ^ "Types do not unify in conditional expression"))
    | GreaterThanEqual (x,y,lc) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error ((Reporting.get_line_and_column lc) ^ "Types do not unify in conditional expression"))
    | EqualTo (x,y,lc) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error ((Reporting.get_line_and_column lc) ^ "Types do not unify in conditional expression"))
    | And (x,y,_) | Or(x,y,_) -> let () = infer_relexpr declarations x in let () = infer_relexpr declarations y in ()
    | Rackets (x,_) -> let () = infer_relexpr declarations x in ()
							     
  (*
    
    TODO: MAYBE, we might need this type-checking, but simple type
    inference is being done in Simple and the dimensions are being
    matched in constant propogation

  *)
  (* let infer_call_argument declarations = function *)
  (*   | CallAddrressedArgument x ->  *)
  (*     if (exists declarations (get_addressed_symbol x)) then *)
  (* 	(get_typed_type (get declarations (get_addressed_symbol x))) *)
  (*     else raise (Error (("Variable " ^ (get_addressed_symbol x)) ^ " is unbound")) *)
  (*   | CallSymbolArgument x ->  *)
  (*     if (exists declarations (get_symbol x)) then *)
  (* 	(get_typed_type (get declarations (get_symbol x))) *)
  (*     else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound")) *)

  (* let rec infer_call_argument_list declarations = function *)
  (*   | h::t -> infer_call_argument declarations h :: infer_call_argument_list declarations t *)
  (*   | [] -> [] *)

  (* let infer_filtercall declarations = function *)
  (*   | Call (x,y) ->  *)
  (*     try  *)
  (* 	let (args,ret) = Hashtbl.find filter_signatures x in *)
  (* 	let arg_types = infer_call_argument_list declarations y in *)
  (*     (\*Now match the filter signature with the arg_types and also if *)
  (* 	everything unifies then just give back the output list *\) *)
  (* 	if (List.length args) <> (List.length arg_types) then *)
  (* 	  raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))) *)
  (* 	else  *)
  (* 	  (\* fix this make a hashmap *\) *)
  (* 	  (\* If the args has a concrete type then they should match, *)
  (* 	     else if it is a poly type then the poly type needs to get *)
  (* 	     replaced with a concrete type in a hashmap. Once this is *)
  (* 	     done then you need to replace the poly types in the output *)
  (* 	     set with the concrete types, after this replacement not *)
  (* 	     even a single poly type should remain in the output set (ret) *\) *)
  (* 	  let tbl = Hashtbl.create 10 in *)
  (* 	  List.iter2 (fun p -> fun y ->  *)
  (* 	    (match y with  *)
  (* 	      | DataTypes.Poly _ as s -> Hashtbl.add tbl s p *)
  (* 	      | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))) *)
  (* 	      | _ as s -> if p <> s then raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))) *)
  (* 	    ) *)
  (* 	  ) arg_types args; *)
  (* 	(\*Now replace all the ret*\) *)
  (* 	  let rr = List.map (fun p ->  *)
  (* 	    match p with *)
  (* 	      | DataTypes.Poly _ as s -> (try Hashtbl.find tbl s with Not_found -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))) *)
  (* 	      | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))) *)
  (* 	      | _ as s -> s) ret in *)
  (* 	  let () = List.iter (fun p -> match p with DataTypes.Poly _ | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))) *)
  (* 	    | _ -> ()) rr in rr *)
  (*     with  *)
  (* 	| Not_found -> raise (Error (("Filter " ^ (get_symbol x)) ^ " is unbound")) *)

  let infer_expr declarations = function
    | SimExpr x -> [infer_simp_expr declarations x]
    | FCall (x,_) -> [] (* check the comment above *)
	
  let rec is_there counter = function
    | h::t -> if counter = h then true else (is_there counter t)
    | [] -> false

  let rec rem_from_dim to_rem_list counter = function
    | h::t -> (if not(is_there counter to_rem_list) then [h] else []) @ (rem_from_dim to_rem_list (counter+1) t)
    | [] -> []

  let is_expr = function
    | TStar | TStarStar -> false
    | _ -> true

  let rec remove_from_dim_list torem list = function
    | h::t ->
      (match h with
	| BracDim x -> remove_from_brac_list torem list x); 
      torem := !torem+1;
      remove_from_dim_list torem list t
    | [] -> ()

  and remove_from_brac_list torem list = function
    | h::t ->
      (match h with
	| DimSpecExpr x -> 
	  if is_expr x then 
	    let () = IFDEF DEBUG THEN print_endline ("Will be dropped: " ^ ((string_of_int !torem))) ELSE () ENDIF in
	    (list:= !torem::!list));
      if not (t = []) then torem := !torem + 1;
      remove_from_brac_list torem list t
    | [] -> ()
      
  let set_ass_decs declarations = function
    | AllTypedSymbol x -> Simple.add_to_declarations x declarations
    | _ -> ()

  let rec infer_stmt declarations = function
    | VarDecl (x,lc) as s -> 
      let () = Simple.add_to_declarations x declarations in s
    | Assign (x,y,lc) as s ->
      if (match y with FCall _ -> false | _ -> true) then
	let expr_type = infer_expr !declarations y in
	if List.length x <> List.length expr_type then
	  raise (Error ((Reporting.get_line_and_column lc) ^ "First_order_type: Input and output types do not unify"))
	else (infer_assign_lvalue_list declarations expr_type 0 x; s)
      else 
	(* Set all the typed declarations in the declarations *)
	(* FIXME: This is a short-cut, in reality some constraints
	   should be solved for primitive types, making sure that the
	   polymorphic functions become concrete. Constraints only flow
	   one way in this case from callee site to the called function,
	   i.e., upwards!! *)
	let () = List.iter (fun r -> set_ass_decs declarations r) x in 
	(* First get the declarations from the declarations list *)
	let fargs = (match y with 
	  | FCall (x,e) -> if not e then (match x with Call (_,x,lc) -> 
	    List.map (fun x -> (match x with | CallAddrressedArgument x -> Simple.get_addressed_symbol x 
	      | CallSymbolArgument x -> Simple.get_symbol x)) x)
	    else []
	  | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "Reached SimExpr after filtering for it"))) in
	let fargdecs = List.map (fun x -> Simple.get !declarations x) fargs in
	let fvardecsyms = List.map (fun x -> (match x with AllAddressedSymbol x -> Simple.get_addressed_symbol x
	  | AllSymbol x -> Simple.get_symbol x | AllTypedSymbol x -> Simple.get_typed_symbol x)) x in
	let fvardecs = List.map (fun x -> Simple.get !declarations x) fvardecsyms in
	(* Put the fvardecs and fargdecs in a hashmap *)
	let () = IFDEF DEBUG THEN print_endline "ADDING TO HASHMAP" ELSE () ENDIF in
	let () = IFDEF DEBUG THEN print_endline (Dot.dot_stmt s) ELSE () ENDIF in
	let torep = (fvardecs@fargdecs) in
	let () = IFDEF DEBUG THEN List.iter (fun x -> print_endline (Dot.dot_typed_symbol x)) torep ELSE () ENDIF in
	let () = Hashtbl.add fcall_map s torep in s
    | Block (x,lc) as s ->
      let vcopy = !declarations in
      let _ = infer_stmt_list declarations x in
      declarations := (Simple.get_block_less_declarations !declarations vcopy); s
    | Split (x,lc) as s -> let _ = infer_stmt declarations x in s
    | CaseDef (x,lc) as s -> let _ = infer_casedef declarations x in s
    | Par (x,y,z,lc) | For (x,y,z,lc) as s ->
      let _ = infer_simp_expr !declarations y in (* we don't bother with colon expr return types *)
      let _ = infer_stmt declarations z in s
    | _ as s -> s (* Escape code, Noop, etc *)
  and infer_casedef declarations = function
    | Case (x,y,lc) as s -> let _ = infer_casecluase_list declarations x in let _ =  infer_otherwise declarations y in s
  and infer_casecluase_list declarations = function
    | h::t -> infer_caseclause declarations h :: infer_casecluase_list declarations t
    | [] -> []
  and infer_caseclause declarations = function
    | Clause (expr,stmt,lc) as s ->
      let () = (infer_relexpr !declarations expr) in
      (* The above inference can have the side-affect of changing the declarations list *)
      let _ = infer_stmt declarations stmt in s
  and infer_otherwise declarations = function
    | Otherwise (x,lc) as s ->
      let _ = infer_stmt declarations x in s
  and infer_stmt_list declarations = function
    | h::t -> infer_stmt declarations h :: infer_stmt_list declarations t
    | [] -> []
  and infer_assign_lvalue_list declarations expr_type counter = function
    | h::t -> infer_assign_lvalue declarations (List.nth expr_type counter) h; infer_assign_lvalue_list declarations expr_type (counter+1) t
    | [] -> ()
  and infer_assign_lvalue declarations expr_type = function
    (* Return back vardeclared or non-var declared symbols *)
    | AllTypedSymbol x ->
      if (Simple.unify_types expr_type (get_first_order_type x)) then 
	let () = IFDEF DEBUG THEN print_endline ("Adding to the declarations: " ^ (Simple.get_typed_symbol x)) ELSE () ENDIF in
	Simple.add_to_declarations x declarations
      else 
	let () = print_types (get_first_order_type x) expr_type in
	raise (Error ((Reporting.get_line_and_column (Simple.get_typed_symbol_lc x)) ^ 
			 "First_order_type: Types do not unify for assignment to " ^ (Simple.get_typed_symbol x)))
    | AllSymbol x ->
      if (Simple.exists !declarations (Simple.get_symbol x)) then
	if (Simple.unify_types expr_type (get_first_order_type (Simple.get !declarations (Simple.get_symbol x)))) then ()
	else 
	  let () = print_types  (get_first_order_type (Simple.get !declarations (Simple.get_symbol x))) expr_type in
	  raise (Error ((Reporting.get_line_and_column (Simple.get_symbol_lc x)) ^ 
			   "First_order_type: Types do not unify for assignment to " ^ (Simple.get_typed_symbol (Simple.get !declarations (Simple.get_symbol x)))))
      else raise (Error (("Variable " ^ (Simple.get_symbol x)) ^ " is unbound"))
    (* TODO: Drop the dimensions that are being dereferenced -- DONE*)
    | AllAddressedSymbol x ->
      if (Simple.exists !declarations (Simple.get_addressed_symbol x)) then
	let (dim_type, dim_list) = (get_first_order_type (Simple.get !declarations (Simple.get_addressed_symbol x))) in
	let dims = (let to_rem_list = ref [] in
		    let torem = ref 0 in
		    remove_from_dim_list torem to_rem_list (match x with AddressedSymbol (_,_,y,_) -> y);
		    rem_from_dim !to_rem_list 0 dim_list) in
	let () = print_dims dims in
	if (Simple.unify_types expr_type (dim_type,dims)) then ()
	else 
	  let () = print_types (dim_type,dims) expr_type in
	  raise (Error ((Reporting.get_line_and_column (Simple.get_addressed_symbol_lc x)) ^ 
			   "First_order_type: Types do not unify for assignment to " ^ 
			   (Simple.get_typed_symbol (Simple.get !declarations (Simple.get_addressed_symbol x)))))
      else raise (Error (("Variable " ^ (Simple.get_addressed_symbol x)) ^ " is unbound"))
	
  let get_vars = function
    | VarDecl (x,_)  -> [x]
    | Assign (x,_,_) -> List.flatten (List.map (fun x -> (match x with AllTypedSymbol x -> [x] | _ -> [])) x)
    (* | Block x -> List.flatten (List.map (fun x -> get_vars x) x) *)
    | _ -> []

  let rec get_dvars = function
    | Block (x,_) -> List.flatten (List.map (fun x -> get_vars x) x)
    | Split (x,_) -> get_dvars x
    | For (x,y,z,lc) | Par (x,y,z,lc) -> 
      let sym = (SimTypedSymbol (DataTypes.Int32s, x,lc)) in
      sym :: (get_vars z)
    | CaseDef (case,_) -> 
      let (clause_list,other) = (match case with Case (x,y,lc) -> (x,y)) in
      let clause_stmt_list = List.map (fun x -> (match x with Clause (_,x,_) -> x)) clause_list in
      let other_stmt = (match other with Otherwise (x,_) -> x) in
      (List.flatten (List.map (fun x -> get_vars x) clause_stmt_list)) @ (get_vars other_stmt)
    | _ -> []
	
  let rec remove_from_declarations declarations dvars = 
    List.iter (fun r -> declarations := List.filter (fun x -> x <> r) !declarations) dvars

  let rec infer_cfg declarations = function
    | Startnode (stmt,x) -> Startnode(stmt, (infer_cfg declarations x))
    (* We need to infer these stmts *)
    | Squarenode (stmt,x) ->
      let () = IFDEF DEBUG THEN print_endline "Entering square node" ELSE () ENDIF in
      let () = IFDEF DEBUG THEN print_endline (Dot.dot_stmt stmt) ELSE () ENDIF in
      let s = (infer_stmt declarations stmt) in
      Squarenode(s,(infer_cfg declarations x))
    | Conditionalnode (relexpr,y,z) ->
      let () = infer_relexpr !declarations relexpr in
      let ny = (infer_cfg declarations y) in
      let nz =(infer_cfg declarations z) in 
      Conditionalnode(relexpr, ny, nz)
    | Backnode x as s -> s
    | Empty as s -> let () = IFDEF DEBUG THEN print_endline "Reached the end" ELSE () ENDIF in s
    | Endnode (stmt,x,z) -> 
      (* FIXME: Need to get rid of all the declared variables like in constant propogation *)
      let dvars = get_dvars stmt in
      (* remove dvars from the declarations list *)
      let () = IFDEF DEBUG THEN print_endline ("End node: " ^ Dot.dot_stmt stmt) ELSE () ENDIF in
      let () = IFDEF DEBUG THEN List.iter (fun x -> print_endline ("Will remove : " ^ Dot.get_typed_symbol x)) dvars ELSE() ENDIF in
      let () = remove_from_declarations declarations dvars in
      Endnode(stmt,(infer_cfg declarations x),z)
      
  let rec get_ins_and_outs = function
    | Squarenode (x,y) -> x::get_ins_and_outs y
    | Empty -> []
    | _ -> raise (Internal_compiler_error "get_ins_and_outs INS/OUTS are not of Squarenode type, CFG construction error")

  let dot_typed_symbol = function
    | SimTypedSymbol (x,y,_) -> (Simple.get_symbol y)
    | ComTypedSymbol (x,y,_) -> (Dot.get_addressed_string y)
      
  let rec replace_in_ou counter type_list = function
    | Squarenode (x,y) -> 
      let tt = (List.nth type_list !counter) in
      counter := !counter + 1;
      let t = (match tt with | SimTypedSymbol (x,_,_) | ComTypedSymbol (x,_,_) -> x) in
      let x = (match x with 
	| VarDecl (x,_) as s ->
	  (match x with 
	    | SimTypedSymbol (x,y,lc) -> 
	      let () = IFDEF DEBUG THEN print_endline "Trying to replace in function signature" ELSE () ENDIF in
	      let () = IFDEF DEBUG THEN print_endline (Simple.get_symbol y) ELSE () ENDIF in
	      (match x with DataTypes.Poly _ -> 
		let () = IFDEF DEBUG THEN print_endline ("Replacing poly with : " ^ (DataTypes.print_datatype t)) ELSE () ENDIF in
		VarDecl (SimTypedSymbol (t,y,lc),lc) | _ -> s)
	    | ComTypedSymbol (x,y,lc) -> (match x with DataTypes.Poly _ -> VarDecl (ComTypedSymbol (t,y,lc),lc) | _ -> s))
	| _ -> raise (Internal_compiler_error "Inputs/Outputs not of type VarDecl")) in
      Squarenode (x, replace_in_ou counter type_list y)
    | Empty as s -> s
    | _ -> raise (Internal_compiler_error "get_ins_and_outs INS/OUTS are not of Squarenode type, CFG construction error")

  let rec replace_ins_outs counter type_list = function
    | h::t -> replace_in_ou counter type_list h :: replace_ins_outs counter type_list t
    | [] -> []

  let infer_topnode check = function
    | Topnode (x,t,constraints,y) ->
      let () = IFDEF DEBUG THEN print_endline ("Filter: " ^ t) ELSE () ENDIF in
      (* First look at the fcall_map and see if you are there, you have to be there whatever happens !! *)
      let filter_signature = 
	(try
	   Hashtbl.find fcall_map x
	 with
	   | Not_found -> 
	     if not check then []
	     else raise (Internal_compiler_error ("Signature for filter " ^ t ^ " not found in the hashtbl"))) in
      (* Now replace the inputs and outputs with the correct primitive types *)
      let temp = List.rev y in
      let y = replace_ins_outs (ref 0) (filter_signature) [(List.nth temp 0) ; (List.nth temp 1)] in
      (* Now put the cfg_stmts in the y list *)
      let y = y @ [(List.nth temp 2)] in
      (* let ret = Topnode (x,t, (List.map ((fun x -> fun y -> infer_cfg x y) (ref [])) (List.rev y))) in *)
      let () = print_string (("Filter " ^ t) ^ " : ") in
      let ret = Topnode (x,t, constraints,(List.map ((fun x -> fun y -> infer_cfg x y) (ref [])) y)) in
      (* Give out the final type signature for this thing *)
      (* let ins = (get_ins_and_outs (List.nth (List.rev y) 0)) in *)
      let ins = (get_ins_and_outs (List.nth y 0)) in
      let () = List.iter (fun x -> print_string ((match x with VarDecl (x,lc) -> (Dot.dot_typed_symbol x) | 
      _ -> raise (Internal_compiler_error ("Found a non-square node for in/outs "))) ^ " ")) ins in
      let () = if ins <> [] then print_string " -> " in
      let () = List.iter (fun x -> print_string ((match x with VarDecl (x,lc) -> (Dot.dot_typed_symbol x ^ " ") | 
      _ -> raise (Internal_compiler_error ("Found a non-square node for in/outs "))))) (get_ins_and_outs (List.nth y 1)) in
      let () = print_endline " " in
      ret
    | Null -> raise (Internal_compiler_error "ERROR (First_order type inference): Topnode is Null")

  let rec infer_filternode check = function
    | Filternode (x,y) ->
      let topnode = infer_topnode check x in
      let ll = infer_rest y in
      Filternode(topnode,ll)
  and infer_rest = function
    | h::t -> infer_filternode true h :: infer_rest t
    | [] -> []

end
