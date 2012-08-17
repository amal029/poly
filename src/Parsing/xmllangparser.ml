module Xmllangparser : sig
  exception StgLangParseError of string ;;
  type t
  val make : unit -> t
  val parse : t -> file:string -> Language.Language.ast
  val write : t -> file:string -> unit
end = 
struct
  open Language;;
  open Language;;
  exception StgLangParseError of string;;

  (* The internal types *)
  type internal_type =
    | Past of ast
    | PtoplevelStmt of toplevelStmt
    | Pfilter of filter
    | PfilterCall of filterCall
    (* | PcallArgument of callArgument *)
    | Potherwise of otherwise
    | PcaseClause of caseClause
    | Pcase of case
    | Pstmt of stmt
    | PrelExpr of relExpr
    | PsimpleExpr of simpleExpr
    | Pvalue of value
    | PtypedSymbol of typedSymbol
    | PaddressedSymbol of addressedSymbol
    | Pindex of index
    | Psymbol of symbol
    | PdataTypes of DataTypes.t
    | Ptunablevariable of AccessPattern.tunablevariable
    | Paccesspattern of AccessPattern.accesspattern
    | Paccesspatternlist of AccessPattern.accesspattern list
    | PPCDATA of string
    | Pinputtypedsymbollist of typedSymbol list
    | Poutputtypedsymbollist of typedSymbol list
    | Pinputcalltypedsymbollist of callArgument list
    | Poutputcalltypedsymbollist of callArgument list
    | Pstmtlist of stmt list
    | Ptoplevelstmtlist of toplevelStmt list
    | Pclauselist of caseClause list
    | Psizelist of AccessPattern.tunablevariable list
    | Pindexlist of index list
  ;;
  type t_t = {xml_parser_t:XmlParser.t;
	      mutable xml:Xml.xml}
  ;;
  type t =
      StgLangType of t_t
  ;;
  let make () = StgLangType {xml_parser_t = XmlParser.make(); xml = Xml.Element ("",[("","")],[]);}
  ;;
  let rec build_program children = 
    if List.length children <> 1 then raise (StgLangParseError ("Error: Ast has too many toplevel children statements:" 
					       ^ (string_of_int (List.length children))));
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Ptoplevelstmtlist x -> res2 := x :: !res2 | _ -> ()) res;
    Past (Program (List.flatten !res2))
  and build_def children = 
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many Def children statements");
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Pfilter x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Pfilter x -> x) res in *)
    PtoplevelStmt (Def (List.hd !res2))
  and build_def_main children = 
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many Defmain children statements");
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Pfilter x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Pfilter x -> x) res in *)
    PtoplevelStmt (DefMain (List.hd !res2))
  and build_top_escape children = 
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many topescape children statements");
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PPCDATA x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with PPCDATA x -> x) res in *)
    PtoplevelStmt (TopEscape (List.hd !res2))
  and build_escape children = 
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many escape children statements");
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PPCDATA x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with PPCDATA x -> x) res in *)
    Pstmt (Escape (List.hd !res2))
  and build_noop children = 
    if children <> [] then raise (StgLangParseError "Error: Ast has too many noop children statements");
    PtoplevelStmt (Noop)
  and build_filter children = 
    if List.length children <> 4 then raise (StgLangParseError "Error: Ast has too many filter children statements");
    let res = List.map build_ast children  in
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    let input_list = ref [] in List.iter (fun x -> match x with Pinputtypedsymbollist x -> input_list := x | _ -> ()) res;
    let output_list = ref [] in List.iter (fun x -> match x with Poutputtypedsymbollist x -> output_list := x | _ -> ()) res;
    let stmt_list = ref [] in List.iter (fun x -> match x with Pstmtlist x -> stmt_list := x | _ -> ()) res;
    Pfilter (Filter (!symbol, !input_list, !output_list, !stmt_list))
  and build_input children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PtypedSymbol x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with PtypedSymbol x -> x) res in *)
    Pinputtypedsymbollist (!res2)
  and build_output children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PtypedSymbol x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with PtypedSymbol x -> x) res in *)
    Poutputtypedsymbollist (!res2)
  and build_typed_symbol children = 
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many typed symbol children statements");
    let res = List.map build_ast children in
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    let tt = ref DataTypes.None in List.iter (fun x -> match x with PdataTypes x -> tt := x | _ -> ()) res;
    PtypedSymbol (TypedSymbol (!tt, !symbol))
  (* special removing the "type" node*)
  and build_type children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many type children statements");
    let res = List.map build_ast children in
    List.hd res
  and build_block children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Psizelist x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Psizelist x -> x) res in *)
    Paccesspattern (AccessPattern.Block (List.flatten !res2))
  and build_border children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Psizelist x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Psizelist x -> x) res in *)
    Paccesspattern (AccessPattern.Border (List.flatten !res2))
  and build_window children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Psizelist x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Psizelist x -> x) res in *)
    Paccesspattern (AccessPattern.Window (List.flatten !res2))
  and build_stride children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Psizelist x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Psizelist x -> x) res in *)
    Paccesspattern (AccessPattern.Stride (List.flatten !res2))
  and build_tiles children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Psizelist x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Psizelist x -> x) res in *)
    Paccesspattern (AccessPattern.Tiles (List.flatten !res2))
  and build_size_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Ptunablevariable x -> res2 := x :: !res2 | _ -> ()) res;
    (* let res2 = List.map (fun x -> match x with Ptunablevariable x -> x) res in *)
    Psizelist (!res2)
  and build_tunable_variables children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many size children statements");
    let res = List.map build_ast children in
    match List.hd res with
      | PPCDATA x -> 
	(try
	  (* try giving back and integer type *)
	   Ptunablevariable (AccessPattern.Integer (int_of_string x))
	 with
	   | Failure "int_of_string" -> Ptunablevariable (AccessPattern.String x))
      | _ -> raise (StgLangParseError "Error: Size has wrong type of children")
  and build_call children = 
    if List.length children <> 3 then raise (StgLangParseError "Error: Ast has too many filterCall children statements");
    let res = List.map build_ast children in
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    let input_list = ref [] in List.iter (fun x -> match x with Pinputcalltypedsymbollist x -> input_list := x | _ -> ()) res;
    let output_list = ref [] in List.iter (fun x -> match x with Poutputcalltypedsymbollist x -> output_list := x | _ -> ()) res;
    PfilterCall (Call (!symbol, !input_list, !output_list))
  and build_addressed_symbol children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many addressedSymbol children statements");
    let res = List.map build_ast children in
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    let index_list = ref [] in List.iter (fun x -> match x with Pindexlist x -> index_list := x | _ -> ()) res;
    PaddressedSymbol (`AddressedSymbol (!symbol,!index_list))
  and build_symbol children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many symbol children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PPCDATA x -> x | _ -> raise (StgLangParseError "Error: Symbol child wrong type")) res in
    Psymbol (`Symbol (List.hd res2))
  and build_otherwise children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many otherwise children statements");
    let res = List.map build_ast children in
    let stmt_list = ref [] in List.iter (fun x -> match x with Pstmtlist x -> stmt_list := x | _ -> ()) res;
    Potherwise (Otherwise !stmt_list)
  and build_clause children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many caluse children statements");
    let res = List.map build_ast children in
    let prel_expr = List.find (fun x -> match x with PrelExpr _ -> true | _ -> false) res in
    let rel_expr = (match prel_expr with PrelExpr x -> x | 
	_ -> raise (StgLangParseError "rel expr of wrong type")) in
    let pstmt_list = List.find (fun x -> match x with Pstmtlist _ -> true | _ -> false) res in
    let stmt_list = (match pstmt_list with Pstmtlist x -> x | 
	_ -> raise (StgLangParseError "stmt list not of type stmt list")) in
    PcaseClause (Clause (rel_expr,stmt_list))
  and build_clause_list children =
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PcaseClause x -> x | 
	_ -> raise (StgLangParseError "case cluause list not of type case Clause")) res in
    Pclauselist (res2)
  and build_case children =
    if List.length children <> 3 then raise (StgLangParseError "Error: Ast has too many case children statements");
    let res = List.map build_ast children in
    let psymbol = List.find (fun x -> match x with Psymbol _ -> true | _ -> false) res in
    let pcaseclauselist = List.find (fun x -> match x with Pclauselist _ -> true | _ -> false) res in
    let potherwise = List.find (fun x -> match x with Potherwise _ -> true | _ -> false) res in
    let symbol = (match psymbol with Psymbol x -> x | 
	_ -> raise (StgLangParseError "case symbol not of type symbol")) in
    let caseClauselist = (match pcaseclauselist with Pclauselist x -> x | 
	_ -> raise (StgLangParseError "case clauses not of type caseClauselist")) in
    let otherwise = (match potherwise with Potherwise x -> x| 
	_ -> raise (StgLangParseError "case otherwise clause not of type otherwise")) in
    Pcase (Case (symbol,caseClauselist,otherwise))
  and build_assign_sym children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many assign symbol children statements");
    let res = List.map build_ast children in
    let psimple_expr = List.find (fun x -> match x with PsimpleExpr _ -> true | _ -> false) res in
    let simple_expr = (match psimple_expr with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "assign sym simple_expr not of type simple expr")) in
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    Pstmt (AssignSym (!symbol,simple_expr))
  and build_assign_loc children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many assign loc children statements");
    let res = List.map build_ast children in
    let psimple_expr = List.find (fun x -> match x with PsimpleExpr _ -> true | _ -> false) res in
    let simple_expr = (match psimple_expr with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "assign bloc simple_expr not of type simple expr")) in
    let psymbol = List.find (fun x -> match x with PaddressedSymbol _ -> true | _ -> false) res in
    let symbol = (match psymbol with PaddressedSymbol x -> x | 
	_ -> raise (StgLangParseError "assign bloc symbol not of type symbol")) in
    Pstmt (AssignLoc (symbol,simple_expr))
  and build_var_decl children =
    (* Ask andrew to fix this too, currently we are missing the typedsymbol node , once fixed compare with 1*)
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many var decl children statements");
    let res = List.map build_ast children in
    let _type = ref DataTypes.None in List.iter (fun x -> match x with PdataTypes x -> _type := x|_ ->()) res;
    let symbol = ref (`Symbol "") in List.iter (fun x -> match x with Psymbol x -> symbol := x | _ -> ()) res;
    Pstmt (VarDecl (TypedSymbol (!_type , !symbol)))
  and build_fcall children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many fcall children statements");
    let res = List.map build_ast children in
    let pfiltercall = List.find (fun x -> match x with PfilterCall _ -> true | _ -> false) res in
    let filtercall = (match pfiltercall with PfilterCall x -> x | 
	_ -> raise (StgLangParseError "Filtercall in case definition is not of type case")) in
    Pstmt (FCall filtercall)
  and build_case_def children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many fcall children statements");
    let res = List.map build_ast children in
    let pfiltercall = List.find (fun x -> match x with Pcase _ -> true | _ -> false) res in
    let filtercall = (match pfiltercall with Pcase x -> x | 
	_ -> raise (StgLangParseError "Filtercall in case definition is not of type case")) in
    Pstmt (CaseDef filtercall)
  (* just return the expression inside it *)
  and build_rel_expr children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many relexpr children statements");
    let res = List.map build_ast children in
    List.hd res
  and build_less_than children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many less than children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::_ -> (h,t) | 
	_ -> raise (StgLangParseError "less than does not have enough exprs")) in
    PrelExpr (LessThan (expr1,expr2))
  and build_greater_then children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many greater than children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) | _ -> 
      raise (StgLangParseError "greater than does not have enough exprs")) in
    PrelExpr (GreaterThan (expr1,expr2))
  and build_equal_to children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many equal to children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) | _ -> 
      raise (StgLangParseError "equal to does not have enough exprs")) in
    PrelExpr (EqualTo (expr1,expr2))
  and build_plus children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many plus children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) 
      | _ -> raise (StgLangParseError "plus does not have enough exprs")) in
    PsimpleExpr (Plus (expr1,expr2))
  and build_minus children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many minus children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t)| 
	_ -> raise (StgLangParseError "minus does not have enough exprs")) in
    PsimpleExpr (Minus (expr1,expr2))
  and build_times children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many times children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) | 
	_ -> raise (StgLangParseError "times does not have enough exprs")) in
    PsimpleExpr (Times (expr1,expr2))
  and build_div children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many div children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "div expr of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) | 
	_ -> raise (StgLangParseError "div does not have enough exprs")) in
    PsimpleExpr (Div (expr1,expr2))
  and build_pow children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many pow children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x |
	_ -> raise (StgLangParseError "pow of wrong type")) res in
    let (expr1,expr2) = (match res2 with h::t::[] -> (h,t) 
      | _ -> raise (StgLangParseError "Error: Ast has too many pow children statements")) in
    PsimpleExpr (Pow (expr1,expr2))
  and build_const children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many const children statements");
    let res = List.map build_ast children in
    let _type = ref DataTypes.None in List.iter (fun x -> match x with PdataTypes x -> _type := x | _ -> ()) res;
    let pvalue = List.find (fun x -> match x with Pvalue _ -> true | _ -> false) res in
    let value = (match pvalue with Pvalue x -> x |
	_ -> raise (StgLangParseError "const value of wrong type")) in
    PsimpleExpr (Const (!_type,value))
  and build_value children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many value children statements");
    let res = List.map build_ast children in
    Pvalue (List.hd (List.map (fun x -> match x with PPCDATA x -> x |
	_ -> raise (StgLangParseError "value of wrong type")) res)) 
  and build_var_ref children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many VarRef children statements");
    let res = List.map build_ast children in
    PsimpleExpr (VarRef (List.hd (List.map (fun x -> match x with Psymbol x -> x | 
	_ -> raise (StgLangParseError "var ref of wrong type")) res))) 
  and build_addr_ref children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many AddrRef children statements");
    let res = List.map build_ast children in
    PsimpleExpr (AddrRef (List.hd (List.map (fun x -> match x with PaddressedSymbol x -> x |
	_ -> raise (StgLangParseError "addr ref of wrong type")) res))) 
  and build_brackets children = 
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many bracket children statements");
    let res = List.map build_ast children in
    let res2 = List.map (fun x -> match x with PsimpleExpr x -> x 
      | _ -> raise (StgLangParseError "Brackets child is of wrong type")) res in
    PsimpleExpr (Brackets (List.hd res2))
  and build_cast children =
    if List.length children <> 2 then raise (StgLangParseError "Error: Ast has too many Cast children statements");
    let res = List.map build_ast children in
    let ptype = List.find (fun x -> match x with PdataTypes _ -> true | _ -> false) res in
    let _type = (match ptype with PdataTypes x -> x | 
	_ -> raise (StgLangParseError "simple _type of wrong type")) in
    let psimpleexpr = List.find (fun x -> match x with PsimpleExpr _ -> true | _ -> false) res in
    let simple_expr = (match psimpleexpr with PsimpleExpr x -> x | 
	_ -> raise (StgLangParseError "simple expr of wrong type")) in
    PsimpleExpr (Cast (_type,simple_expr))
  and build_index children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many Cast children statements");
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PPCDATA x -> res2 := x :: !res2 | _ -> ()) res;
    Pindex (int_of_string (List.hd !res2))
  and build_index_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Pindex x -> res2 := x :: !res2 | _ -> ()) res;
    Pindexlist (!res2)
  and build_access_pattern children =
    if List.length children <> 1 then raise (StgLangParseError "Error: Ast has too many accesspattern children statements");
    let res = List.map build_ast children in
    List.hd res
  and build_access_pattern_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Paccesspattern x -> res2 := x :: !res2 | _ -> ()) res;
    Paccesspatternlist (!res2)
  and build_aggregate children =
    if List.length children <> 3 then raise (StgLangParseError "Error: Ast has too many aggregate children statements");
    let res = List.map build_ast children in
    let size_list = ref [] in List.iter (fun x -> match x with Psizelist x -> size_list := x | _ -> ()) res;
    let accesspatternlist = ref [] in List.iter (fun x -> match x with Paccesspatternlist x -> accesspatternlist := x | _ ->()) res; 
    let _type = ref DataTypes.None in List.iter (fun x -> match x with PdataTypes x -> _type := x | _ -> ()) res;
    PdataTypes (DataTypes.Aggregate (!size_list, !accesspatternlist, !_type))
  and build_input_call_argument_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PaddressedSymbol x -> res2 := (x :> callArgument) :: !res2 | _ -> ()) res;
    let res3 = ref [] in List.iter (fun x -> match x with Psymbol x -> res3 := (x :> callArgument) :: !res3 | _ -> ()) res;
    Pinputcalltypedsymbollist (!res2  @ !res3 )
  and build_output_call_argument_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PaddressedSymbol x -> res2 := (x :> callArgument) :: !res2 | _ -> ()) res;
    let res3 = ref [] in List.iter (fun x -> match x with Psymbol x -> res3 := (x :> callArgument) :: !res3 | _ -> ()) res;
    Poutputcalltypedsymbollist ((!res2 @ !res3))
  and build_stmt_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with Pstmt x -> res2 := (x :: !res2) | _ -> ()) res;
    Pstmtlist (!res2)
  and build_toplevel_stmt_list children =
    let res = List.map build_ast children in
    let res2 = ref [] in List.iter (fun x -> match x with PtoplevelStmt x -> res2 := (x :: !res2) | _ -> ()) res;
    Ptoplevelstmtlist (!res2)
  and build_ast = function
    | Xml.PCData x -> PPCDATA x
    | Xml.Element (name,_,children_list) -> 
      (match name with
	(* The stg language definition *)
      	| "Program" -> build_program children_list (*done*)
      	| "Def" -> build_def children_list(*done*)
      	| "DefMain" -> build_def_main children_list(*done*)
      	| "Topescape" -> build_top_escape children_list(*done*)
      	| "Noop" -> build_noop children_list(*done*)
      	| "Filter" -> build_filter children_list(*done*)
      	| "Call" -> build_call children_list(*done*)
	| "AddressedSymbol" -> build_addressed_symbol children_list(*done*)
	| "Symbol" -> build_symbol children_list(* done *)
	| "Otherwise" -> build_otherwise children_list(* done *)
	| "Clause" -> build_clause children_list(* done *)
	| "Case" -> build_case children_list(* done *)
	| "AssignSym" -> build_assign_sym children_list(* done *)
	| "AssignLoc" -> build_assign_loc children_list(* done *)
	| "VarDecl" -> build_var_decl children_list(* done *)
	| "FCall" -> build_fcall children_list(* done *)
	| "CaseDef" -> build_case_def children_list(* done *)
	| "Escape" -> build_escape children_list(*done*)
	| "LessThan" -> build_less_than children_list(*done*)
	| "GreaterThan" -> build_greater_then children_list(*done*)
	| "EqualTo" -> build_equal_to children_list(*done*)
	| "Plus" -> build_plus children_list(*done*)
	| "Minus" -> build_minus children_list(*done*)
	| "Times" -> build_times children_list(*done*)
	| "Div" -> build_div children_list(*done*)
	| "Pow" -> build_pow children_list(*done*)
	| "Const" -> build_const children_list(*done*)
	| "VarRef" -> build_var_ref children_list(*done*)
	| "AddrRef" -> build_addr_ref children_list(*done*)
	| "Brackets" -> build_brackets children_list(*done*)
	| "Cast" -> build_cast children_list(*done*)
	| "Value" -> build_value children_list(*done*)
	| "TypedSymbol" -> build_typed_symbol children_list(*done*)
	| "Index" -> build_index children_list(*done*)
	(* The access patterns *)
	| "DatatypeConst" -> Paccesspattern (AccessPattern.Const) (*done*)(* Ask andrew to change this *)
	| "Simple" -> Paccesspattern (AccessPattern.Simple)(*done*)
	| "Map" -> Paccesspattern (AccessPattern.Map) (*done*)
	| "Block" -> build_block children_list(*done*)
	| "Border" -> build_border children_list(*done*)
	| "Window" -> build_window children_list(*done*)
	| "Stride" -> build_stride children_list(*done*)
	| "Tiles" -> build_tiles children_list(*done*)
	| "Size" ->   build_tunable_variables children_list (*done*)(* => tunablevariable*)
	(* The data types *)
	| "Type" -> build_type children_list (*done*)(* this is excess baggage*)
	| "Int8" -> PdataTypes (DataTypes.Int8)(*done*)
	| "Int16" -> PdataTypes (DataTypes.Int16)(*done*)
	| "Int32" -> PdataTypes (DataTypes.Int32)(*done*)
	| "Int64" -> PdataTypes (DataTypes.Int64)(*done*)
	| "Int8s" -> PdataTypes (DataTypes.Int8s)(*done*)
	| "Int16s" -> PdataTypes (DataTypes.Int16s)(*done*)
	| "Int32s" -> PdataTypes (DataTypes.Int32s)(*done*)
	| "Int64s" -> PdataTypes (DataTypes.Int64s)(*done*)
	| "Float8" -> PdataTypes (DataTypes.Float8)(*done*)
	| "Float16" ->PdataTypes (DataTypes.Float16)(*done*)
	| "Float32" ->PdataTypes (DataTypes.Float32)(*done*)
	| "Float64" ->PdataTypes (DataTypes.Float64)(*done*)
	(* | "Float8s" ->PdataTypes (DataTypes.Float8s)(\*done*\) *)
	(* | "Float16s" ->PdataTypes (DataTypes.Float16s)(\*done*\) *)
	(* | "Float32s" ->PdataTypes (DataTypes.Float32s)(\*done*\) *)
	(* | "Float64s" -> PdataTypes (DataTypes.Float64s)(\*done*\) *)
	(* | "Filter" -> build_filter children_list *) (* how to differentiate from filter in the language *)
	| "Aggregate" -> build_aggregate children_list(*done*)
	| "AccessPatternList" -> build_access_pattern_list children_list (* done *)(* => accesspatternlist *)
	| "AccessPattern" -> build_access_pattern children_list (* done *) (* this is excess baggage *)
	| "None" -> PdataTypes (DataTypes.None)(*done*)
	(* specials *)
	| "InputList" -> build_input children_list (*done*)(* => typedsymbol list *)
	| "OutputList" -> build_output children_list (*done*)(* => typedsymbol list *)
	| "StmtList" -> build_stmt_list children_list (* => stmt list *)
	| "ToplevelStmtList" -> build_toplevel_stmt_list children_list (* => toplevelStmt list, ask andrew to change this *)
	| "ClauseList" ->   build_clause_list children_list(* done *) (* => caseClause list *)
	| "SizeList" ->   build_size_list children_list (*done*)(* => tunablevariable list *)
	| "IndexList" ->   build_index_list children_list(*done*) (* => index list *)
	(* the one below is used inside of call arguments, but it cannot be differentiated from filter definition,
	   needs to change*)
	| "CallInputList" -> build_input_call_argument_list children_list (* done *)(* => callArgumentList *)
	| "CallOutputList" -> build_output_call_argument_list children_list (* done *)(* => callArgumentList *)
	| "RelExpr" -> build_rel_expr children_list (* superflous *)
	| _ -> raise (StgLangParseError ("Error: unknown node type:" ^ name))
      )
  and parse (parser:t) ~file =
    match parser with StgLangType x ->
      let xml = XmlParser.parse x.xml_parser_t (XmlParser.SFile file) in
      (match parser with 
	| StgLangType x -> x.xml <- xml);
      (match (build_ast xml) with Past x -> x | _ -> raise (StgLangParseError "Error: Wrong Ast top node"))
  ;;
  let write (parser:t) ~file = 
    match parser with 
      | StgLangType x -> 
	let ret = Xml.to_string_fmt x.xml in
	(* open a file descriptor to the named file *)
	let ochan = open_out file in
	output_string ochan ret;
	flush ochan; 
	close_out ochan
  ;;
end
