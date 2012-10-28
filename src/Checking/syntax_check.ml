(* 
   Author: Avinash Malik
   Date: Fri Apr 27 18:57:29 IST 2012
   This modules only matches variable names. 
   Filter names are not checked in this file 
*)

module VarnameCheck = 
struct
  open Language
  open Language
  exception SyntaxException of string

  let match_vars lc vars used errors = 
    let m = ref false in
    for i = 1 to (List.length used) do
      m := false;
      let u = List.nth used i in
      for j = 1 to (List.length !vars) do
	let v = List.nth !vars j in
	if (u = v) then m := true
      done;
      if !m = false then errors:=(u::!errors);
    done

  let get_symbol = function
    | Symbol (x,_) -> x
  let get_addressed_symbol = function
    | AddressedSymbol (x,_,_,_) -> get_symbol x

  let  get_symbols = function
    | SimTypedSymbol (_,y,_) -> get_symbol y
    | ComTypedSymbol (_,y,_) -> get_addressed_symbol y

  let get_all_syms vars used = function
    | AllAddressedSymbol x -> used:=(get_addressed_symbol x)::!used
    | AllSymbol x -> used:=(get_symbol x)::!used
    | AllTypedSymbol x -> vars:=(get_symbols x)::!vars

  let get_call_args = function
    | CallAddrressedArgument x -> get_addressed_symbol x
    | CallSymbolArgument x -> get_symbol x

  let rec get_callarguments = function
      | h::t -> get_call_args h::get_callarguments t
      | [] -> []

  let check_filter_call vars errors warns = function
    | Call (_,y,lc) -> match_vars lc vars (get_callarguments y) errors

  let rec check_sim_expr vars errors warns = function
      | Plus (x,y,_) | Minus (x,y,_) 
      | Times(x,y,_) | Div (x,y,_) 
      | Pow (x,y,_) -> check_sim_expr vars errors warns x; 
	check_sim_expr vars errors warns y
      | VarRef (x,lc) -> match_vars lc vars [(get_symbol x)] errors 
      | Brackets (x,_) | Opposite (x,_) -> check_sim_expr vars errors warns x
      | ColonExpr (x,y,z,_) -> check_sim_expr vars errors warns x; 
	check_sim_expr vars errors warns y;
	check_sim_expr vars errors warns z;
      | _ -> ()

  let check_expr vars errors warns = function
      | FCall (x,e) -> if not e then check_filter_call vars errors warns x
      | SimExpr x -> check_sim_expr vars errors warns x
      (* | Main -> () *)

  let rec check_relexpr vars errors warns = function
      | LessThan (x,y,_) | LessThanEqual (x,y,_) | GreaterThan (x,y,_) 
      | GreaterThanEqual (x,y,_) | EqualTo (x,y,_) -> 
	(check_sim_expr vars errors warns x;
	 check_sim_expr vars errors warns y)
      | And (x,y,_) | Or (x,y,_) -> 
	let () = check_relexpr vars errors warns x in 
	let () = check_relexpr vars errors warns y in ()
      | Rackets (x,_) -> check_relexpr vars errors warns x
	  
  let rec check_scope_existence vs errors = function
    | h::t -> check_in_vs h errors vs; check_scope_existence vs errors t
    | [] -> ()
  and check_in_vs name errors = function
    | h::t -> if name = h then errors:=name::!errors
      else check_in_vs name errors t
    | [] -> ()

  let rec check_stmt vars errors warns = function
      | Assign (x,y,lc,_) -> let used = ref [] in
			let vs = ref [] in
			get_all_symbols vs used x;
			check_scope_existence !vs errors !vars;
			match_vars lc vars !used errors;
			vars:=(!vars @ !vs);
			check_expr vars errors warns y
      | VarDecl (x,lc) -> let vs = ref [] in
		     let sym = get_symbols x in
		     vs:=sym::!vs; 
		     check_scope_existence !vs errors !vars;
		     match_vars lc vars !vs warns;
		     vars:=(!vars @ !vs)
      | CaseDef (x,lc) -> check_case vars errors warns x
      | Block (t,lc) -> let vcopy = !vars in check_block vars errors warns t; vars := vcopy
      | Par (x,y,z,lc) | For (x,y,z,lc) -> vars:=(get_symbol x)::!vars; 
	check_sim_expr vars errors warns y; 
	check_stmt vars errors warns z
      | _ -> ()
  and check_block vars errors warns = function
      | h::t -> check_stmt vars errors warns h; check_block vars errors warns t
      | [] -> ()
  and get_all_symbols vars used = function
      | h::t -> get_all_syms vars used h; get_all_symbols vars used t
      | [] -> ()
  and check_case vars errors warns = function
      | Case (x,y,lc) -> 
	(match y with Otherwise (x,_) -> check_stmt vars errors warns x);
	check_caseclause_list vars errors warns x
  and check_caseclause_list vars errors warns = function
      | h::t -> check_clause vars errors warns h;  
	check_caseclause_list vars errors warns t
      | [] -> ()
  and check_clause vars errors warns = function
    | Clause (x,y,_) -> check_stmt vars errors warns y;
	check_relexpr vars errors warns x

  let rec print_probs = function
      | h::t -> print_string h; print_probs t
      | [] -> ()

  let rec check_filter = function
      | Filter (fname, ins , outs, stmt,sp) -> 
	(* First build the argument list to this filter *)
	let ints = get_args ins in
	let outs = get_args outs in
	let errors = ref [] in
	let warns = ref [] in
	let vars = ref (ints@outs) in
	check_stmt vars errors warns stmt;
	(* Now print the error and warning statements *)
	if ((List.length !errors) >= 1) then
	  begin
	    print_endline (("ERROR: Fitler " ^ (get_symbol fname)) ^
			      " could not find these variable names in scope or names multiply defined : ");
	    print_probs !errors;
	  end;
	if ((List.length !warns) >= 1) then
	  begin
	    print_endline (("WARNING: In Fitler " ^ (get_symbol fname)) ^
		 " filter arguments are over shadowed by internal variables");
	    print_probs !warns;
	  end
  and get_args = function
      | h::t -> (get_symbols h) ::  get_args t
      | [] -> []
	
  let rec check_tolevelstmt = function
      | Def (x,_,lc) -> check_filter x
      | DefMain (x,_,lc) -> check_filter x
      | _ -> () (* You can put anything in the escape code *)

  let rec check_ast = function
    | Program x -> check_program x
  and check_program = function
    | h::t -> check_tolevelstmt h; check_program t;
    | [] -> ()
end
