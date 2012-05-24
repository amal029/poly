
module Simple =
struct
  open Language
  open Language
  exception Error of string;;
  exception Internal_compiler_error of string;;


  let filter_signatures = Hashtbl.create 10;;

  let get_symbol = function
    | Symbol x -> x
  let get_addressed_symbol = function
    | AddressedSymbol (x,_,_) -> get_symbol x
  let get_typed_symbol = function
    | SimTypedSymbol (_,x) -> get_symbol x
    | ComTypedSymbol (_,x) -> get_addressed_symbol x

  let exists declarations s = 
    List.exists (fun x -> (get_typed_symbol x) = s ) declarations
  let get declarations s = 
    List.find (fun x -> (get_typed_symbol x) = s) declarations
  let get_typed_type = function
    | SimTypedSymbol (x,_) 
    | ComTypedSymbol (x,_) -> (match x with 
	(* | DataTypes.None -> raise (Error "Type of variable undefined, before use")  *)
	(* | DataTypes.Poly x -> raise (Error "Type of the variable polymorphic (needs to be concrete), before use")  *)
	| _ as s -> s)

  let add_to_declarations s declarations = 
    if (exists !declarations (get_typed_symbol s)) then 
      raise (Error (("Symbol "^ (get_typed_symbol s)) ^ "multiply defined" ))
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
    | SimTypedSymbol (x,_) -> unify_types x t 
    | ComTypedSymbol (x,_) -> unify_types x t

  let ispoly = function
    | SimTypedSymbol (x,_) | ComTypedSymbol (x,_) -> match x with | DataTypes.Poly _ -> true | _ -> false

  let isnone = function
    | SimTypedSymbol (x,_) | ComTypedSymbol (x,_) -> match x with | DataTypes.None -> true | _ -> false

  let build_new_typed_symbol expr_type = function
    | SimTypedSymbol (_,x) -> SimTypedSymbol(expr_type,x)
    | ComTypedSymbol (_,x) -> ComTypedSymbol(expr_type,x)
	
  let replace_in_declaration declarations s neww = 
    let ll = List.filter (fun x -> x <> s) !declarations in
    declarations := neww :: ll

  let rec get_block_less_declarations declarations = function
    | h::t -> (List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol h)) declarations) :: get_block_less_declarations declarations t
    | [] -> []
      
  let infer_call_argument declarations = function
    | CallAddrressedArgument x -> 
      if (exists declarations (get_addressed_symbol x)) then
	(get_typed_type (get declarations (get_addressed_symbol x)))
      else raise (Error (("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | CallSymbolArgument x -> 
      if (exists declarations (get_symbol x)) then
	(get_typed_type (get declarations (get_symbol x)))
      else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound"))
  let rec infer_call_argument_list declarations = function
    | h::t -> infer_call_argument declarations h :: infer_call_argument_list declarations t
    | [] -> []
      
  (*We might have to change the types on the arguments in the FCall *)
  let get_call_decs declarations = function
    | CallAddrressedArgument x -> 
      if (exists declarations (get_addressed_symbol x)) then
	(get declarations (get_addressed_symbol x))
      else raise (Error (("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | CallSymbolArgument x -> 
      if (exists declarations (get_symbol x)) then
	get declarations (get_symbol x)
      else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound"))
  let rec get_call_decs_list declarations = function
    | h::t -> get_call_decs declarations h :: get_call_decs_list declarations t
    | [] -> []
      
  
  let rec resolve_all_decs_types orig neww = function
    | SimTypedSymbol (x,y) as s -> if x = orig then SimTypedSymbol (neww,y) else s
    | ComTypedSymbol (x,y) as s -> if x = orig then ComTypedSymbol (neww,y) else s

  let infer_filtercall declarations = function
    | Call (x,y) -> 
      try 
	(* let () = print_endline (get_symbol x) in *)
	let (args,ret) = Hashtbl.find filter_signatures x in
	let arg_types = infer_call_argument_list !declarations y in
	let call_decs = ref (get_call_decs_list !declarations y) in
	(* let () = List.iter (fun x -> print_endline ("Inputs: " ^ (DataTypes.print_datatype x)))  arg_types in *)
	(* let () = List.iter (fun x -> print_endline ("Outputs: " ^ (DataTypes.print_datatype x)))  ret in *)
      (*Now match the filter signature with the arg_types and also if
	everything unifies then just give back the output list *)
	if (List.length args) <> (List.length arg_types) then
	  raise (Error ("Filter arguments and signature do not unify " ^ (get_symbol x)))
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
		raise (Error ("Filter arguments and signature do not unify one of them is not defined yet " ^ (get_symbol x)))
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
			 raise (Error ("Filter arguments and signature do not unify " ^ (get_symbol x)))
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
			   raise (Error ("Filter arguments and signature do not unify " ^ (get_symbol x)))) 
	    );
	    counter := !counter + 1;
	  ) arg_types args; (* args are the filter begin called type signature *)
	  (*Now replace all the ret*)
	  let rr = List.map (fun p ->
	    match p with
	      | DataTypes.Poly _ as s -> (try Hashtbl.find tbl s with Not_found -> 
		let () = print_types s s in
		raise (Error ("Filter arguments and signature do not unify " ^ (get_symbol x))))
	      | DataTypes.None -> 
		raise (Error ("Filter arguments and signature do not unify, data type output is None: " ^ (get_symbol x)))
	      | _ as s -> s) ret in
	  let () = List.iter (fun p -> match p with (* DataTypes.Poly _ *) | DataTypes.None -> 
	    let () = print_types p p in
	    raise (Error ("Filter arguments and signature do not unify " ^ (get_symbol x)))
	    | _ -> ()) rr in rr
      with 
	| Not_found -> raise (Error (("Filter " ^ (get_symbol x)) ^ " is unbound"))
	  
  let rec get_used_decs declarations = function
    | VarRef x -> [get declarations (get_symbol x)]
    | AddrRef x -> [get declarations (get_addressed_symbol x)]
    | Plus (x,y) | Minus (x,y) | Times (x,y)
    | Div (x,y) | Pow (x,y) -> get_used_decs declarations x @ get_used_decs declarations y
    | Brackets x -> get_used_decs declarations x
    | _ -> raise (Internal_compiler_error "Simple type inference erroneously hits a a non-poly while infering math expression")

  let rec infer_simp_expr declarations = function
    | Const (x,_) -> x
    | Plus (x,y) | Minus (x,y) | Times (x,y) 
    | Div (x,y) | Pow (x,y) -> 
      let ltype = infer_simp_expr declarations x in
      let rtype = infer_simp_expr declarations y in
      if (unify_types ltype rtype) then ltype
      else
	(*If any one of the parts is poly then make it non poly??
	  This should not be done for None  --> FIXME
	*)
	(match (ltype,rtype) with
	  | (DataTypes.None, _) | (_,DataTypes.None) -> 
	    let () = print_types ltype rtype in
	    raise (Error "Mathematical operation branches do not unify one of them is none")
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
	    raise (Error "Mathematical operation branches do not unify"))
    | VarRef x -> 
	if (exists !declarations (get_symbol x)) then
	  (get_typed_type (get !declarations (get_symbol x)))
	else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound"))
    | AddrRef x -> 
	if (exists !declarations (get_addressed_symbol x)) then
	  (get_typed_type (get !declarations (get_addressed_symbol x)))
	else raise (Error (("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
    | Brackets x -> infer_simp_expr declarations x
    | Cast (x,_) -> x
    | Opposite x -> infer_simp_expr declarations x
    | ColonExpr (x,y,z) -> 
      let f = (infer_simp_expr declarations x) in
      let s = (infer_simp_expr declarations y) in
      let t = (infer_simp_expr declarations z) in
      if (List.exists (fun x -> x = f) DataTypes.floating) || (List.exists (fun x -> x = s) DataTypes.floating) 
	|| (List.exists (fun x -> x = t) DataTypes.floating) then
	raise (Error "Colon expressions can only of integral type")
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
    | Main -> raise (Internal_compiler_error "Simple type inference hits a Main")
    (* TODO 1.) Lookup the hashtbl of filter signatures a.) If filter
       signature exists then return the complete signature expression,
       because it is of type -> b.) If it does not exist then leave it
       as none it will be inferred later on. Once the signture is
       inferred later on we will need then need to traverse the ast
       again filling in the changing declarations and types again.

       c.) or just force people to write things before they are invoked
       like in Ocaml -> for now I am taking this approach*)
    | FCall x -> 
      (* let  () = print_string ("Checking FCALL ") in *)
      infer_filtercall declarations x

  let infer_relexpr declarations = function
    | LessThan (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let () = print_types lexpr_type rexpr_type in
	raise (Error "Types do not unify in conditional expression")
    | LessThanEqual (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let () = print_types lexpr_type rexpr_type in
	raise (Error "Types do not unify in conditional expression")
    | GreaterThan (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let () = print_types lexpr_type rexpr_type in
	raise (Error "Types do not unify in conditional expression")
    | GreaterThanEqual (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let () = print_types lexpr_type rexpr_type in
	raise (Error "Types do not unify in conditional expression")
    | EqualTo (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else 
	let () = print_types lexpr_type rexpr_type in
	raise (Error "Types do not unify in conditional expression")

  let rec replace_var_decls declarations = function
    | VarDecl x as ret -> 
      let v = x in
      (match x with
	| SimTypedSymbol (x,_) 
	| ComTypedSymbol (x,_) -> 
	  (match x with
	    | DataTypes.None -> 
	      (try 
		 let w = (List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol v)) declarations) in
		 (match w with 
		   | SimTypedSymbol(DataTypes.None,_) 
		   | ComTypedSymbol(DataTypes.None,_) -> 
		     let () = print_endline (("Warning: Symbol " ^ (get_typed_symbol w)) ^ " not being used") in Noop
		   | SimTypedSymbol(_,_)
		   | ComTypedSymbol(_,_) as s -> VarDecl s
		 )
	       with
		 | Not_found -> raise (Internal_compiler_error (("Simple type inference (second pass, cannot find " ^ (get_typed_symbol v)) ^ " in declarations")))
	    | _ -> ret))
    | Block x -> Block (replace_block_stmts declarations x)
    | For (x,y,z) -> For(x,y, (replace_var_decls declarations z))
    | Par(x,y,z) -> Par(x,y, (replace_var_decls declarations z))
    | CaseDef x -> CaseDef (replace_casedef declarations x)
    | _ as s -> s
  and replace_block_stmts declarations = function
    | h::t -> replace_var_decls declarations h :: replace_block_stmts declarations t
    | [] -> []
  and replace_casedef declarations = function
    | Case (x,y) -> Case ((replace_clauselist declarations x), (replace_otherwise declarations y))
  and replace_clauselist declarations = function
    | h::t -> replace_clause declarations h :: replace_clauselist declarations t
    | [] -> []
  and replace_clause declarations = function
    | Clause (x,s) -> 
      let r = replace_var_decls declarations s in
      let rr = (match r with | VarDecl x -> Noop | _ as s -> s) in
      Clause (x,rr)
  and replace_otherwise declarations = function
    | Otherwise s -> 
      let r = replace_var_decls declarations s in
      let rr = (match r with | VarDecl x -> Noop | _ as s -> s) in
      Otherwise (replace_var_decls declarations rr)

  let rec infer_stmt declarations = function
    | VarDecl x as s -> 
      (* let () = print_endline ("vardecl: " ^ (get_typed_symbol x)) in *)
      let () = add_to_declarations x declarations in s
    | Assign (x,y) -> 
      let expr_type = infer_expr declarations y in
      if List.length x <> List.length expr_type then 
	raise (Error "Simple: Input and output types do not unify")
      else 
	Assign ( (infer_assign_lvalue_list declarations expr_type 0 x) , y)
    | Block x -> 
      (* let () = print_endline ("Checking block") in *)
      let vcopy = !declarations in 
      let ll = infer_stmt_list declarations x in
      let rett_stmts = replace_var_decls !declarations (Block ll) in 
      declarations := (get_block_less_declarations !declarations vcopy);
      rett_stmts
      (* We need to replace the var decls with the proper types in here *)
    | CaseDef x -> CaseDef (infer_casedef declarations x)
    | Par (x,y,z) -> 
      let _ = infer_simp_expr declarations y in (* we don't bother with colon expr return types *)
      (*We need to declare the index variable in the declarations and
	then remove it once we are done*)
      let vcopy = !declarations in
      declarations := SimTypedSymbol(DataTypes.Int32s , x)  :: !declarations;
      let ss = (match z with | VarDecl _ -> Noop | _ -> infer_stmt declarations z) in
      declarations := (get_block_less_declarations !declarations vcopy); (*Be carefule, something might have changed inside *)
      Par(x,y,ss)
    | For (x,y,z) -> 
      let _ = infer_simp_expr declarations y in
      (*We need to declare the index variable in the declarations and
	then remove it once we are done*)
      let vcopy = !declarations in
      declarations := SimTypedSymbol(DataTypes.Int32s , x)  :: !declarations;
      let ss = (match z with | VarDecl _ -> Noop | _ -> infer_stmt declarations z) in
      declarations := (get_block_less_declarations !declarations vcopy); (*Be carefule, something might have changed inside *)
      For(x,y,ss)
    | _ as s -> s
  and infer_casedef declarations = function
    | Case (x,y) -> Case ((infer_casecluase_list declarations x), (infer_otherwise declarations y))
  and infer_casecluase_list  declarations = function
    | h::t -> infer_caseclause declarations h :: infer_casecluase_list declarations t
    | [] -> []
  and infer_caseclause declarations = function
    | Clause (expr,stmt) -> 
      let () = (infer_relexpr declarations expr) in
      (* The above inference can have the side-affect of changing the declarations list *)
      let ss = (match stmt with | VarDecl x -> Noop | _ -> (infer_stmt declarations stmt)) in
      Clause (expr, ss)
  and infer_otherwise declarations = function
    | Otherwise x -> 
      let ss = (match x with | VarDecl x -> Noop | _ -> (infer_stmt declarations x)) in
      Otherwise ss
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
      (* let () = print_endline (get_typed_symbol x) in *)
      if (match_typed_symbol_type expr_type x) then
	(add_to_declarations x declarations; s)
      else 
	if (isnone x) then
	  let neww = build_new_typed_symbol expr_type x in
	  replace_in_declaration declarations x neww;
	  AllTypedSymbol (neww)
	else
	  begin
	    if (not (ispoly x)) then
	      let () = (match expr_type with 
		| DataTypes.Poly _ -> 
		  (* Propogate a concrete type from lvalue to the rvalue *)
		  declarations := List.map (resolve_all_decs_types expr_type (get_typed_type x)) !declarations;
		| _ -> let () = print_types (get_typed_type x) expr_type in
		       raise (Error (("types do not unify for assignment to " ^ (get_typed_symbol x))))) in s
	    else
	      let () = print_types (get_typed_type x) expr_type in
	      raise (Error (("types do not unify for assignment to " ^ (get_typed_symbol x)))) 
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
			 raise (Error (("types do not unify for assignment to " ^ (get_symbol x))))) in s
	      else
		let () = print_types (get_typed_type (get !declarations (get_symbol x))) expr_type in
		raise (Error (("types do not unify for assignment to " ^ (get_symbol x))))
	    end 
      else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound"))
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
			 raise (Error (("types do not unify for assignment to " ^ (get_addressed_symbol x))))) in s
	      else
		let () = print_types (get_typed_type (get !declarations (get_addressed_symbol x))) expr_type in
		raise (Error (("types do not unify for assignment to " ^ (get_addressed_symbol x))))
	    end
	    (* let () = print_types (get_typed_type (get !declarations (get_addressed_symbol x))) expr_type in *)
	    (* raise (Error ("types do not unify for assignment to " ^ (get_addressed_symbol x))) *)
      else raise (Error (("Variable " ^ (get_addressed_symbol x)) ^ " is unbound"))
      (* else (\* This means this is a declaration lets add it to the declarations list *\) *)
      (* 	let ret = ComTypedSymbol (expr_type, x) in  *)
      (* 	add_to_declarations ret declarations; AllTypedSymbol (ret) *)

  (* This is called in the second pass, gets rid of all the variables,
     which are not used *)
  (* Making the vardecls for Addressed dimensions *)
  let get_dimspec = function
    | DimSpecExpr x -> 
      (match x with
	| VarRef x -> [(SimTypedSymbol (DataTypes.Int32s,x))]
	| Const _ -> []
	| _ -> raise (Error "Dimensions of addressed symbols in function parameters cannot be anything names of consts"))
  let rec get_dimspec_list = function
    | h::t -> get_dimspec h @ get_dimspec_list t
    | [] -> []
  let get_brac_dims_2 = function
    | BracDim x -> get_dimspec_list x
  let rec get_brac_dim_list = function
    | h::t -> get_brac_dims_2 h @ get_brac_dim_list t
    | [] -> []
  let get_dims_2 = function
    | AddressedSymbol (_,_,x) -> get_brac_dim_list x
  let get_dim_param = function
    | SimTypedSymbol (_,_) -> []
    | ComTypedSymbol (_,y) -> 
      (* let () = print_endline ("getting dims from: " ^ (get_typed_symbol s)) in *)
      get_dims_2 y
  let rec get_dim_params = function
    | h::t -> 
      (* let () = print_endline "Checking" in *)
      get_dim_param h @ get_dim_params t
    | [] -> []

  let infer_filter = function
    | Filter (x,y,z,w) ->
      let () = print_string (("Filter " ^ (get_symbol x)) ^ " : ") in
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
	Hashtbl.add filter_signatures x ((List.map (fun x -> (get_typed_type x)) zi), (List.map (fun x -> (get_typed_type x)) zo));
	let f = Filter(x,zi,zo,ret_stmt) in f
      with
	| Not_found -> raise (Internal_compiler_error "Filter re-build declarations don't have all the outputs")

  let infer_toplevelstmt = function
    | Def x -> Def (infer_filter x)
    | DefMain x -> DefMain(infer_filter x)
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
  
  (* This function only works on declarations and hence, if the
     declarations is a SimTypedSymbol then that means it is a scalar.
     If it is a ComTypedSymbol then we need to give all the dimensions
     in the format mentioned above.  *)
  let get_brac_dims = function
    | DimSpecExpr x ->
      (match x with
	| Const (x,y) ->
	  if not (List.exists (fun r -> x = r) DataTypes.unsignedIntegral) then
	    raise (Error "First_order_type_inference: Dimensions of array not of type unsignedIntegral")
	  else y
	| _ -> raise (Internal_compiler_error "First_order_type_inference hits non-const inside dimensions after constant folding!!"))

  let get_dims = function
    | BracDim x -> List.map get_brac_dims x

  let get_first_order_type = function
    | SimTypedSymbol (x,_) -> (x,[])
    (* Remember we have gotten rid of all the angledim <> lists, so that
       we inly have [] and [,,,][] expressions qith drop semnatics
       left *)
    | ComTypedSymbol (x,y) -> (x, List.flatten (List.map get_dims (match y with AddressedSymbol (_,_,y) -> y)))
      
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
    | h::t -> to_drop_bracdim counter_list counter h; to_drop_dims counter_list (counter+1) t
    | [] -> ()

  let get_to_drop = function
    | AddressedSymbol (_,_,x) -> let p = ref [] in to_drop_dims p 0 x; !p
      
  let rec drop todrop counter dims = function
    | h::t -> if not (List.exists (fun x -> x = counter) todrop) then dims := h :: !dims; drop todrop (counter+1) dims t
    | [] -> ()

  let rec infer_simp_expr declarations = function
    | Const (x,_) -> (x,[])
    | Plus(x,y) | Minus(x,y) | Times (x,y) | Div (x,y)
    | Pow(x,y) ->
      let lv = infer_simp_expr declarations x in
      let rv = infer_simp_expr declarations y in
      if (Simple.unify_types lv rv) then lv
      else raise (Error "First order dimensions do not unify in the mathematical operation")
    | VarRef x ->
	if (Simple.exists declarations (Simple.get_symbol x)) then
	  (get_first_order_type (Simple.get declarations (Simple.get_symbol x)))
	else raise (Error (("Variable " ^ (Simple.get_symbol x)) ^ " is unbound"))
    | AddrRef x ->
	if (Simple.exists declarations (Simple.get_addressed_symbol x)) then
	  let dec_type = (get_first_order_type (Simple.get declarations (Simple.get_addressed_symbol x))) in
	  let to_drop = (get_to_drop x) in
	  if ((List.length to_drop) <= (List.length (snd (dec_type)))) then
	    raise (Internal_compiler_error "First_order_type_inference: dropping more dimensions then the size of the polytope")
	  else
	    let actual_dims = ref [] in
	    (drop to_drop 0 actual_dims (snd dec_type));
	    (fst (dec_type)), !actual_dims
	else raise (Error (("Variable " ^ (Simple.get_addressed_symbol x)) ^ " is unbound"))
    | Brackets x -> infer_simp_expr declarations x
    (*Be careful here: it is not the same as Simple case, because we
      need to get the current type and dimensions of the simple
      expression and then convert the cast to this cast *)
    | Cast (x,y) ->
      let dims = snd (infer_simp_expr declarations y) in
      (x,dims)
    | Opposite x -> infer_simp_expr declarations x
    | ColonExpr (x,y,z) ->
      let f = (infer_simp_expr declarations x) in
      let s = (infer_simp_expr declarations y) in
      let t = (infer_simp_expr declarations z) in
      if ((not (Simple.unify_types (snd (f)) [])) || (not (Simple.unify_types (snd (s)) [])) || (not (Simple.unify_types (snd (t)) []))) then
	raise (Error "First_order_type error: Expressions in colon expressions can only be of sclar types")
      else f
    | TStar | TStarStar -> raise (Internal_compiler_error "First_order_type erroneously reached TStar/TStarStar while doing type inference")

  let infer_relexpr declarations = function
    | LessThan (x,y) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | LessThanEqual (x,y) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | GreaterThan (x,y) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | GreaterThanEqual (x,y) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | EqualTo (x,y) ->
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (Simple.unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
	
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
    | Main -> raise (Internal_compiler_error "First_order_type: erroneously reached a Main type in infer_expr")
    | FCall x -> [] (* check the comment above *)
	
  let rec infer_stmt declarations = function
    | VarDecl x as s -> let () = Simple.add_to_declarations x declarations in s
    | Assign (x,y) as s ->
      if (match y with FCall _ -> false | _ -> true) then
	let expr_type = infer_expr !declarations y in
	if List.length x <> List.length expr_type then
	  raise (Error "First_order_type: Input and output types do not unify")
	else (infer_assign_lvalue_list declarations expr_type 0 x; s)
      else s
    | Block x as s ->
      let vcopy = !declarations in
      let _ = infer_stmt_list declarations x in
      declarations := (Simple.get_block_less_declarations !declarations vcopy); s
    | CaseDef x as s -> let _ = infer_casedef declarations x in s
    | Par (x,y,z) | For (x,y,z) as s ->
      let _ = infer_simp_expr !declarations y in (* we don't bother with colon expr return types *)
      let _ = infer_stmt declarations z in s
    | _ as s -> s (* Escape code, Noop, etc *)
  and infer_casedef declarations = function
    | Case (x,y) as s -> let _ = infer_casecluase_list declarations x in let _ =  infer_otherwise declarations y in s
  and infer_casecluase_list declarations = function
    | h::t -> infer_caseclause declarations h :: infer_casecluase_list declarations t
    | [] -> []
  and infer_caseclause declarations = function
    | Clause (expr,stmt) as s ->
      let () = (infer_relexpr !declarations expr) in
      (* The above inference can have the side-affect of changing the declarations list *)
      let _ = infer_stmt declarations stmt in s
  and infer_otherwise declarations = function
    | Otherwise x as s ->
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
      if (Simple.unify_types expr_type (get_first_order_type x)) then Simple.add_to_declarations x declarations
      else raise (Error ("First_order_type: Types do not unify for assignment to" ^ (Simple.get_typed_symbol x)))
    | AllSymbol x ->
      if (Simple.exists !declarations (Simple.get_symbol x)) then
	if (Simple.unify_types expr_type (get_first_order_type (Simple.get !declarations (Simple.get_symbol x)))) then ()
	else raise (Error ("First_order_type: Types do not unify for assignment to" ^ (Simple.get_symbol x)))
      else raise (Error (("Variable " ^ (Simple.get_symbol x)) ^ " is unbound"))
    | AllAddressedSymbol x ->
      if (Simple.exists !declarations (Simple.get_addressed_symbol x)) then
	if (Simple.unify_types expr_type (get_first_order_type (Simple.get !declarations (Simple.get_addressed_symbol x)))) then ()
	else raise (Error ("First_order_type: Types do not unify for assignment to" ^ (Simple.get_addressed_symbol x)))
      else raise (Error (("Variable " ^ (Simple.get_addressed_symbol x)) ^ " is unbound"))

  let rec infer_cfg declarations = function
    | Startnode (stmt,x) -> Startnode(stmt, (infer_cfg declarations x))
    (* We need to infer these stmts *)
    | Squarenode (stmt,x) ->
      let s = (infer_stmt declarations stmt) in
      Squarenode(s,(infer_cfg declarations x))
    | Conditionalnode (relexpr,y,z) ->
      let () = infer_relexpr !declarations relexpr in
      Conditionalnode(relexpr,(infer_cfg declarations y), (infer_cfg declarations z))
    | Backnode x as s -> s
    | Empty as s -> s
    | Endnode (stmt,x) -> Endnode(stmt,(infer_cfg declarations x))
      
  (* let rec infer_cfg_list declarations = function *)
  (*   | h::t -> infer_cfg declarations h :: infer_cfg_list declarations t *)
  (*   | [] -> [] *)
  
  let infer_topnode = function
    | Topnode (x,t,y) ->
      (* Put the x -> signature_mapping in the filter_signatures Hashtbl*)
      (*MAYBE: We will need to Hashtbl, but the fcall primitive type
	inference is bing done in Simple and the dimensions are being done
	in Const propogation*)
      (* Topnode (x,t, (infer_cfg_list (ref []) y)) *)
      Topnode (x,t, (List.map ((fun x -> fun y -> infer_cfg x y) (ref [])) y))
    | Null -> raise (Internal_compiler_error "ERROR (First_order type inference): Topnode is Null")

  let rec infer_filternode = function
    | Filternode (x,y) ->
      let ll = infer_rest y in
      let topnode = infer_topnode x in Filternode(topnode,ll)
  and infer_rest = function
    | h::t -> infer_filternode h :: infer_rest t

end
