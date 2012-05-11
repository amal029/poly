
(* -------------The type judgments------------------------ *)

(* Environment *)

(* DataTypes.t *)

(* Const := DataTypes.t => 'a[m..] *)

(* Expressions: *)

(* simple expressions *)
(* (+) := DataTypes.t => 'a[m..] -> 'a[m..] -> 'a[m..] = <fun> *)
(* (-) := DataTypes.t => 'a[m..] -> 'a[m..] -> 'a[m..] = <fun> *)
(* (\*\) := DataTypes.t => 'a[m..] -> 'a[m..] -> 'a[m..] = <fun> *)
(* (/) := DataTypes.t => 'a[m..] -> 'a[m..] -> 'a[m..] = <fun> *)
(* (^) := DataTypes.t => 'a[m..] -> 'a[m..] -> 'a[m..] = <fun> *)
(* ([]) := DataTypes.t => 'a[m...] -> 'a[n..], s.t: |'a[m..]| > |'a[n...]|-1 *)
(* (Cast) := DataTypes.t => 'a[m...] -> 'b[m...], s.t: |'a[m..]| = |'b[m..]| *)

(* rel expressions *)
(* (<) := DataTypes.t => 'a[m..] -> 'a[m..] -> bool = <fun> *)
(* (<=) := DataTypes.t => 'a[m..] -> 'a[m..] -> bool = <fun> *)
(* (>) := DataTypes.t => 'a[m..] -> 'a[m..] -> bool = <fun> *)
(* (>=) := DataTypes.t => 'a[m..] -> 'a[m..] -> bool = <fun> *)
(* (==) := DataTypes.t => 'a[m..] -> 'a[m..] -> bool = <fun> *)

(* statements  *)

(* (Noop) := () *)
(* (=) := 'a[m..] -> 'a[m..] = <fun> *)


(* ----------The typing rules---------------------- *)

(* (Case) := If type inference rules from lambda calculus *)
(* (For/Par) := Recursion type inference rules from lambda calculus *)
(* (FCall) := \lambda.e e *)
(* (Escape) := () *)
(* (Def/DefMain) := let rules for type inference (this is the only place polymorphism might happen) *)



(* ------------Rewrites---------------- *)
(* (Block) := rewrite to \lambdae.e with bounded recursion and Case *)


module Simple_local_filter_types =
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
	| DataTypes.None -> raise (Internal_compiler_error "Simple type inference erroneously hits typed_type as polymorphic") 
	| DataTypes.Poly x -> raise (Internal_compiler_error "Simple type inference erroneously hits typed_type as polymorphic") 
	| _ as s -> s)

  let add_to_declarations s declarations = 
    if (exists !declarations (get_typed_symbol s)) then 
      raise (Error (("Symbol "^ (get_typed_symbol s)) ^ "multiply defined" ))
    else declarations := s :: !declarations

  let match_typed_symbol_type t = function
    | SimTypedSymbol (x,_) -> x = t 
    | ComTypedSymbol (x,_) -> x = t

  let isnone = function
    | SimTypedSymbol (x,_) | ComTypedSymbol (x,_) -> match x with | DataTypes.None -> true | _ -> false

  let build_new_typed_symbol expr_type = function
    | SimTypedSymbol (_,x) -> SimTypedSymbol(expr_type,x)
    | ComTypedSymbol (_,x) -> ComTypedSymbol(expr_type,x)
	
  let replace_in_declaration declarations s neww = 
    let ll = List.filter (fun x -> x <> s) !declarations in
    declarations := neww :: ll

  let rec get_block_less_declarations declarations = function
    | h::t -> (List.find (fun x -> x = h) declarations) :: get_block_less_declarations declarations t
    | [] -> []
      
  (*The expressions types are lists or just DataTypes.t types , because
    of -> types, but Ocaml lets one compare two lists and its elements
    with just =. This is also the reason why this function is
    polymorphic*)

  let unify_types l r = l = r
      
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

  let infer_filtercall declarations = function
    | Call (x,y) -> 
      try 
	let (args,ret) = Hashtbl.find filter_signatures x in
	let arg_types = infer_call_argument_list declarations y in
      (*Now match the filter signature with the arg_types and also if
	everything unifies then just give back the output list *)
	if (List.length args) <> (List.length arg_types) then
	  raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))
	else 
	  (* fix this make a hashmap *)
	  (* If the args has a concrete type then they should match,
	     else if it is a poly type then the poly type needs to get
	     replaced with a concrete type in a hashmap. Once this is
	     done then you need to replace the poly types in the output
	     set with the concrete types, after this replacement not
	     even a single poly type should remain in the output set (ret) *)
	  let tbl = Hashtbl.create 10 in
	  List.iter2 (fun p -> fun y -> 
	    (match y with 
	      | DataTypes.Poly _ as s -> Hashtbl.add tbl s p
	      | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))
	      | _ as s -> if p <> s then raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))
	    )
	  ) arg_types args;
	(*Now replace all the ret*)
	  let rr = List.map (fun p -> 
	    match p with
	      | DataTypes.Poly _ as s -> (try Hashtbl.find tbl s with Not_found -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x))))
	      | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))
	      | _ as s -> s) ret in
	  let () = List.iter (fun p -> match p with DataTypes.Poly _ | DataTypes.None -> raise (Error ("Filter arguments and signature do not unify" ^ (get_symbol x)))
	    | _ -> ()) rr in rr
      with 
	| Not_found -> raise (Error (("Filter " ^ (get_symbol x)) ^ " is unbound"))

  let rec infer_simp_expr declarations = function
    | Const (x,_) -> x
    | Plus (x,y) | Minus (x,y) | Times (x,y) 
    | Div (x,y) | Pow (x,y) -> 
      let ltype = infer_simp_expr declarations x in
      let rtype = infer_simp_expr declarations y in
      if (unify_types ltype rtype) then ltype
      else raise (Error "Mathematical operation branches do not unify")
    | VarRef x -> 
	if (exists declarations (get_symbol x)) then
	  (get_typed_type (get declarations (get_symbol x)))
	else raise (Error (("Variable " ^ (get_symbol x)) ^ " is unbound"))
    | AddrRef x -> 
	if (exists declarations (get_addressed_symbol x)) then
	  (get_typed_type (get declarations (get_addressed_symbol x)))
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
	| _ -> false)
      then raise (Error "Colon expressions can only of integral type")
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
    | FCall x -> infer_filtercall declarations x

  let infer_relexpr declarations = function
    | LessThan (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | LessThanEqual (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | GreaterThan (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | LessThan (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | GreaterThanEqual (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")
    | EqualTo (x,y) -> 
      let lexpr_type = (infer_simp_expr declarations x) in
      let rexpr_type = (infer_simp_expr declarations y) in
      if (unify_types lexpr_type rexpr_type) then ()
      else raise (Error "Types do not unify in conditional expression")

  let rec infer_stmt declarations = function
    | Assign (x,y) -> 
      let expr_type = infer_expr !declarations y in
      if List.length x <> List.length expr_type then 
	raise (Error "Input and output types do not unify")
      else Assign ( (infer_assign_lvalue_list declarations expr_type 0 x) , y)
    | Block x -> 
      let vcopy = !declarations in 
      let ll = infer_stmt_list declarations x in 
      declarations := (get_block_less_declarations !declarations vcopy);
      Block ll
    | CaseDef x -> CaseDef (infer_casedef declarations x)
    | Par (x,y,z) -> 
      let _ = infer_simp_expr !declarations y in (* we don't bother with colon expr return types *)
      Par(x,y,(infer_stmt declarations z))
    | For (x,y,z) -> 
      let _ = infer_simp_expr !declarations y in
      For(x,y,(infer_stmt declarations z))
    | _ as s -> s
  and infer_casedef declarations = function
    | Case (x,y) -> Case ((infer_casecluase_list declarations x), (infer_otherwise declarations y))
  and infer_casecluase_list  declarations = function
    | h::t -> infer_caseclause declarations h :: infer_casecluase_list declarations t
    | [] -> []
  and infer_caseclause declarations = function
    | Clause (expr,stmt) -> 
      let () = (infer_relexpr !declarations expr) in
      (* The above inference can have the side-affect of changing the declarations list *)
      Clause (expr, infer_stmt declarations stmt)
  and infer_otherwise declarations = function
    | Otherwise x -> Otherwise (infer_stmt declarations x)
  and infer_stmt_list declarations = function
    | h::t -> infer_stmt declarations h :: infer_stmt_list declarations t 
    | [] -> []
  and infer_assign_lvalue_list declarations expr_type counter = function
    | h::t -> infer_assign_lvalue declarations (List.nth expr_type counter) h :: infer_assign_lvalue_list declarations expr_type (counter+1) t
    | [] -> []
  and infer_assign_lvalue declarations expr_type = function
  (* Return back vardeclared or non-var declared symbols *)
    | AllTypedSymbol x as s -> 
      if (match_typed_symbol_type expr_type x) then 
	(add_to_declarations x declarations; s)
      else 
	if (isnone x) then 
	  let neww = build_new_typed_symbol expr_type x in 
	  replace_in_declaration declarations x neww;
	  AllTypedSymbol (neww)
	else raise (Error ("types do not unify for assignment to" ^ (get_typed_symbol x)))
    | AllSymbol x as s ->
      if (exists !declarations (get_symbol x)) then 
	if (match_typed_symbol_type expr_type (get !declarations (get_symbol x))) then s
	else 
	  if (isnone (get !declarations (get_symbol x))) then
	    let neww = build_new_typed_symbol expr_type (get !declarations (get_symbol x)) in 
	    replace_in_declaration declarations (get !declarations (get_symbol x)) neww;
	    AllTypedSymbol (neww)
	  else raise (Error ("types do not unify for assignment to" ^ (get_symbol x)))
      else (* This means this is a declaration lets add it to the declarations list *)
	let ret = SimTypedSymbol (expr_type, x) in 
	add_to_declarations ret declarations; AllTypedSymbol(ret)
    | AllAddressedSymbol x as s -> 
      if (exists !declarations (get_addressed_symbol x)) then 
	if (match_typed_symbol_type expr_type (get !declarations (get_addressed_symbol x))) then s
	else 
	  if (isnone (get !declarations (get_addressed_symbol x))) then
	    let neww = build_new_typed_symbol expr_type (get !declarations (get_addressed_symbol x)) in 
	    replace_in_declaration declarations (get !declarations (get_addressed_symbol x)) neww;
	    AllTypedSymbol (neww)
	  else raise (Error ("types do not unify for assignment to" ^ (get_addressed_symbol x)))
      else (* This means this is a declaration lets add it to the declarations list *)
	let ret = ComTypedSymbol (expr_type, x) in 
	add_to_declarations ret declarations; AllTypedSymbol (ret)

  let infer_filter = function
    | Filter (x,y,z,w) ->
      let declarations = ref (y @ z) in
      let ret_stmt = infer_stmt declarations w in
      (* First get the outputs from the declarations and put those in this filter*)
      try 
	let zo = List.map (fun y -> List.find (fun x -> (get_typed_symbol x) = (get_typed_symbol y)) !declarations) z in
	Filter(x,y,zo,ret_stmt)
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

(*Once all the inputs and outputs for the filters are infered then we
  re-go through the ast and replace all the none Datatypes with the
  filter inputs and outputs with *)

end
