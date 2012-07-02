open Language
open Language
open CFG
open Llvm
open Llvm_scalar_opts

exception Error of string
exception Internal_compiler_error of string

let context = global_context ()
let the_module = create_module context "poly jit"
let builder = builder context

(* Build the optimization passes *)
let the_fpm = PassManager.create_function the_module
(* Do simple "peephole" optimizations and bit-twiddling optzn. *)
let () = add_instruction_combination the_fpm
(* reassociate expressions. *)
let () =  add_reassociation the_fpm
(* Eliminate Common SubExpressions. *)
let () = add_gvn the_fpm
(* Simplify the control flow graph (deleting unreachable blocks, etc). *)
let () =   add_cfg_simplification the_fpm
(* Perform alias analysis *)
(* let () = add_basic_alias_analysis the_fpm *)
(* let () = add_type_based_alias_analysis the_fpm *)
(* Memory to register promotion *)
let () = add_memory_to_register_promotion the_fpm

(* Initialize the pass manager *)
let _ =  PassManager.initialize the_fpm

(* Putting the jit in*)
let _ = Llvm_executionengine.initialize_native_target
let exec_engine = Llvm_executionengine.ExecutionEngine.create the_module
let () = Llvm_target.TargetData.add (Llvm_executionengine.ExecutionEngine.target_data exec_engine) the_fpm

let fcall_names = Hashtbl.create 10
let func_name_counter = ref 1

let enode = ref Empty

let get_symbol = function
  | Symbol (x,_) -> x
let get_addressed_symbol = function
  | AddressedSymbol (x,_,_,_) -> get_symbol x
let get_typed_symbol = function
  | SimTypedSymbol (_,x,_) -> get_symbol x
  | ComTypedSymbol (_,x,_) -> get_addressed_symbol x

let get_data_types lc = function
  | DataTypes.Int8 | DataTypes.Int16 | DataTypes.Int32 | DataTypes.Int64 
  | DataTypes.Int8s | DataTypes.Int16s | DataTypes.Int32s | DataTypes.Int64s -> "int"
  | DataTypes.Float8 | DataTypes.Float16 | DataTypes.Float32 | DataTypes.Float64 -> "float"
  | _ -> raise (Error ((Reporting.get_line_and_column lc) ^ " datatype not supported in LLVM IR"))

let get_typed_symbol_lc = function
  | SimTypedSymbol (_,_,lc) -> lc
  | ComTypedSymbol (_,_,lc) -> lc
    
let get_typed_type = function
  | SimTypedSymbol (x,_,lc) 
  | ComTypedSymbol (x,_,lc) -> get_data_types lc x

let get_exact_typed_type = function
  | SimTypedSymbol (x,_,lc) 
  | ComTypedSymbol (x,_,lc) -> x


let create_entry_block_alloca f vn t = 
  let builder = builder_at context (instr_begin (entry_block f)) in
  build_alloca t vn builder

let get_llvm_primitive_type lc = function
  | DataTypes.Int8 | DataTypes.Int8s -> i8_type
  | DataTypes.Int16 | DataTypes.Int16s -> i16_type
  | DataTypes.Int32 | DataTypes.Int32s -> i32_type
  | DataTypes.Int64 | DataTypes.Int64s -> i64_type
  | DataTypes.Float8 
  | DataTypes.Float16 
  | DataTypes.Float32 -> float_type
  | DataTypes.Float64 -> double_type
  | _ -> raise (Error ((Reporting.get_line_and_column lc) ^ "DataType cannot be supported in llvm IR"))

let get declarations s = 
  List.find (fun x -> (match x with (x,y) -> (get_typed_symbol x) = s)) declarations

let exists declarations s = 
  List.exists (fun x -> (match x with (x,y) -> (get_typed_symbol x)) = s) declarations

let add_to_declarations s alloca declarations = 
  if (exists !declarations (get_typed_symbol s)) then 
    (* let () = print_curr_line_num !curr_lnum in *)
    raise (Error (((Reporting.get_line_and_column (get_typed_symbol_lc s)) ^ "Symbol "^ (get_typed_symbol s)) ^ " multiply defined" ))
  else declarations := (s,alloca) :: !declarations

let codegen_typedsymbol f declarations = function
  | SimTypedSymbol (x,y,lc) as s -> 
    let datatype = get_llvm_primitive_type lc x in
    let alloca = create_entry_block_alloca f (get_symbol y) (datatype context) in
    (* Add the var decl to the declarations list *)
    add_to_declarations s alloca declarations
  | ComTypedSymbol (_,_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ "complex types currently not supported"))
    
let rec get_exact_simexpr_type declarations = function
  | Plus (x,_,_) | Minus (x,_,_) | Pow (x,_,_) 
  | Div (x,_,_) | Times (x,_,_) | Mod (x,_,_) -> get_exact_simexpr_type declarations x
  | Opposite (x,_) -> get_exact_simexpr_type declarations x
  | Const (x,_,_) -> x
  | Cast (x,_,_) -> x
  | Brackets (x,_) -> get_exact_simexpr_type declarations x
  | VarRef (x,_) -> let (x,_) = get declarations (get_symbol x) in get_exact_typed_type x
  | AddrRef (_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ " complex types not yet supported"))
  | _ -> raise (Internal_compiler_error ("Unsupported simple expr"))

let rec get_simexpr_type declarations = function
  | Plus (x,_,_) | Minus (x,_,_) | Pow (x,_,_) 
  | Div (x,_,_) | Times (x,_,_) | Mod (x,_,_) -> get_simexpr_type declarations x
  | Opposite (x,_) -> get_simexpr_type declarations x
  | Const (x,_,lc) -> get_data_types lc x
  | Cast (x,_,lc) -> get_data_types lc x
  | Brackets (x,_) -> get_simexpr_type declarations x
  | VarRef (x,_) -> let (x,_) = get declarations (get_symbol x) in get_typed_type x
  | AddrRef (_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ " complex types not yet supported"))
  | _ -> raise (Internal_compiler_error ("Unsupported simple expr"))

(* We need to know the exact type of every expression *)
let rec codegen_simexpr declarations = function
  | Plus (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_add (codegen_simexpr declarations x) (codegen_simexpr declarations y) "addtemp" builder
      | "float" -> build_fadd (codegen_simexpr declarations x) (codegen_simexpr declarations y) "addftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Minus (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_sub (codegen_simexpr declarations x) (codegen_simexpr declarations y) "subtemp" builder
      | "float" -> build_fsub (codegen_simexpr declarations x) (codegen_simexpr declarations y) "subftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Times (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_mul (codegen_simexpr declarations x) (codegen_simexpr declarations y) "timestemp" builder
      | "float" -> build_fmul (codegen_simexpr declarations x) (codegen_simexpr declarations y) "timesftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  (* We need the exact type for building the division instruction *)
  | Div (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_sdiv (codegen_simexpr declarations x) (codegen_simexpr declarations y) "divtemp" builder
      | "float" -> build_fdiv (codegen_simexpr declarations x) (codegen_simexpr declarations y) "divftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Pow (x,y,lc) -> raise(Error((Reporting.get_line_and_column lc) ^ " LLVM does not support pow/wow internally :-("))

  | Mod (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_srem (codegen_simexpr declarations x) (codegen_simexpr declarations y) "remtemp" builder 
      | "float" ->build_frem (codegen_simexpr declarations x) (codegen_simexpr declarations y) "remftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Opposite (x,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_neg (codegen_simexpr declarations x) "negtemp" builder
      | "float" -> build_fneg (codegen_simexpr declarations x) "negftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Const (x,v,lc) -> 
    (match get_data_types lc x with
      | "int" -> const_int ((get_llvm_primitive_type lc x) context) (int_of_string v)
      | "float" -> const_float ((get_llvm_primitive_type lc x) context) (float_of_string v)
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Cast (ctype,y,lc) -> 
    (* We need to use one of the many cast operators in llvm *)
    let vtype = get_exact_simexpr_type declarations y in
    (* Now compare them *)
    (match DataTypes.cmp_datatype (vtype,ctype) with
      | "sext" -> build_sext (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "sexttemp" builder
      | "trunc" -> build_trunc (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "trunctemp" builder
      | "fpext" -> build_fpext (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "fpexttemp" builder
      | "fptrunc" -> build_fptrunc (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "fptrunctemp" builder
      | "sitofp" -> build_sitofp (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "sitofptemp" builder
      | "uitofp" -> build_uitofp (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "uitofptemp" builder
      | "fptosi" -> build_fptosi (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "fptositemp" builder
      | "fptoui" -> build_fptoui (codegen_simexpr declarations y) ((get_llvm_primitive_type lc ctype) context) "fptouitemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Brackets (x,_) -> codegen_simexpr declarations x
  | VarRef (x,lc) -> build_load (match get declarations (get_symbol x) with | (_,x) -> x) (get_symbol x) builder
  | AddrRef (_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ "complex types currently not supported"))
  | _ -> raise (Internal_compiler_error "Bogus simple expression hit while producing LLVM IR" )

let rec codegen_relexpr declarations = function
  | LessThan (x,y,lc) -> 
    let lhs = codegen_simexpr declarations x in
    let rhs = codegen_simexpr declarations y in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Slt lhs rhs "Slttemp" builder
      | "float" -> build_fcmp Fcmp.Olt lhs rhs "Olttemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | LessThanEqual (x,y,lc) -> 
    let lhs = codegen_simexpr declarations x in
    let rhs = codegen_simexpr declarations y in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sle lhs rhs "Sletemp" builder
      | "float" -> build_fcmp Fcmp.Ole lhs rhs "Oletemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | GreaterThan (x,y,lc) -> 
    let lhs = codegen_simexpr declarations x in
    let rhs = codegen_simexpr declarations y in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sgt lhs rhs "Sgttemp" builder
      | "float" -> build_fcmp Fcmp.Ogt lhs rhs "Ogttemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | GreaterThanEqual (x,y,lc) -> 
    let lhs = codegen_simexpr declarations x in
    let rhs = codegen_simexpr declarations y in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sge lhs rhs "Sgetemp" builder
      | "float" -> build_fcmp Fcmp.Oge lhs rhs "Ogetemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | EqualTo (x,y,lc) -> 
    let lhs = codegen_simexpr declarations x in
    let rhs = codegen_simexpr declarations y in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Eq lhs rhs "Eqtemp" builder
      | "float" -> build_fcmp Fcmp.Oeq lhs rhs "Oeqtemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | And (x,y,lc) -> 
    let lhs = codegen_relexpr declarations x in
    let rhs = codegen_relexpr declarations y in
    build_and lhs rhs "andtemp" builder
  | Or (x,y,lc) -> 
    let lhs = codegen_relexpr declarations x in
    let rhs = codegen_relexpr declarations y in
    build_and lhs rhs "ortemp" builder
  | Rackets (x,lc) -> codegen_relexpr declarations x

let codegen_callargs lc declarations = function
  | CallSymbolArgument x -> build_load (match get declarations (get_symbol x) with | (_,x) -> x) (get_symbol x) builder
  | CallAddrressedArgument _ -> raise (Error ((Reporting.get_line_and_column lc) ^ "complex types not yet supported in llvm backend"))

let codegen_fcall lc declarations = function
  | FCall x -> 
    (* First lookup the name of the function *)
    let (name,args,lc) =
      (match x with
	| Call (name,y,lc) -> 
	  (* lookup name in fcall_names Hashtbl *)
	  try
	    ((Hashtbl.find fcall_names (get_symbol name)),y,lc)
	  with 
	    | Not_found -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ get_symbol name ^ " not found in hashtbl for fcall names"))) in
    (* Now start building the llvm build_call *)
    let () = IFDEF DEBUG THEN print_endline ("Found function: " ^ name) ELSE () ENDIF in
    let callee = (match lookup_function name the_module with
      | Some callee -> callee
      | None -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ name ^ " function not in llvm module"))) in
    let bargs = List.map (fun x -> codegen_callargs lc declarations x) args in (callee,bargs)
  | SimExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " ended up with SimExpr when it shouldn't"))

let codegen_asssimexpr lc declarations = function
  | SimExpr x ->
    let r = codegen_simexpr declarations x in
    let () = IFDEF DEBUG THEN dump_value r ELSE () ENDIF in r
  | FCall _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " ended up with FCall when it shouldn't"))

let codegen_stmt f declarations = function
  | Assign (ll, expr, lc) -> 
    (* codegen the rhs *)
    let () = IFDEF DEBUG THEN print_endline "assigning" ELSE () ENDIF in
    (* add the declaration to the *)
    (match expr with
      | SimExpr _ -> 
	let rval = codegen_asssimexpr lc !declarations expr in
	List.iter (fun x ->
	  (match x with
	    | AllTypedSymbol x -> 
	      let () = codegen_typedsymbol f declarations x in
	      let (_,alloca) = get !declarations (get_typed_symbol x) in
	      let _ = build_store rval alloca builder in ()
	    | AllSymbol x -> 
	      (* lookup that the symbol is in the declarations list and get it *)
	      let (_,alloca) = get !declarations (get_symbol x) in
	      let _ = build_store rval alloca builder in ()
	    | AllAddressedSymbol _ -> raise (Error ((Reporting.get_line_and_column lc)^ "complex types currently not supported in llvm IR generation")))) ll
      | FCall _ as s -> 
	let (callee,args) = codegen_fcall lc !declarations s in
	(* The assignments types are all alloca types just get the actual alloca *)
	let oargs = List.map (fun x ->
	  (match x with
	    | AllTypedSymbol x -> 
	      let () = codegen_typedsymbol f declarations x in
	      let (_,alloca) = get !declarations (get_typed_symbol x) in alloca
	    | AllSymbol x -> 
	      (* lookup that the symbol is in the declarations list and get it *)
	      let (_,alloca) = get !declarations (get_symbol x) in alloca
	    | AllAddressedSymbol _ -> raise (Error ((Reporting.get_line_and_column lc)^ "complex types currently not supported in llvm IR generation")))) ll in
	(* Now make the call *)
	(* let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) oargs ELSE () ENDIF in *)
	let d =  build_call callee (Array.of_list (args@oargs)) "" builder in ())
	(* let () = IFDEF DEBUG THEN dump_value d ELSE () ENDIF in ()) *)
  | VarDecl (x,lc) -> codegen_typedsymbol f declarations x
  | _ -> raise (Internal_compiler_error "Hit a bogus stmt while compiling to llvm")

(* Generate the body of the function block *)
let rec codegen_cfg f declarations = function
  | Empty -> let () = IFDEF DEBUG THEN print_endline "hit an empty node" ELSE () ENDIF in ()
  | Startnode (stmt,x) -> 
    (match stmt with
      | CaseDef _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a startnode") ELSE () ENDIF in
	let () = codegen_cfg f declarations x in
	(* Now we need to continue with the enode!!*)
	codegen_cfg f declarations !enode
      | _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a startnode") ELSE () ENDIF in
	codegen_cfg f declarations x)
  | Squarenode (stmt,x) ->
    let () = IFDEF DEBUG THEN print_endline ("Hit a squarenode") ELSE () ENDIF in
    let () = codegen_stmt f declarations stmt in
    codegen_cfg f declarations x
  | Endnode (stmt,x,_) as s -> 
    (match stmt with
      | CaseDef _ -> enode := s (* Set yourself up !! *)
      | _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a endnode") ELSE () ENDIF in
	codegen_cfg f declarations x)
  | Backnode x -> raise (Error ("Loops currently not handeled in the LLVM backend"))
  | Conditionalnode (relexpr,tbranch,fbranch) ->
    (* First insert the conditional *)
    let cond_val = codegen_relexpr !declarations relexpr in
    (* First get the current basic block and the parent function *)
    let start_bb = insertion_block builder in
    (* This might be some other basic block*)
    let the_function = block_parent start_bb in
    (* Now append the then basic block to the start_bb*)
    let then_bb = append_block context "tbranch" the_function in
    (* Position the builder to append to the true branch block *)
    let () = position_at_end then_bb builder in
    (* Next build the true branch *)
    let () = codegen_cfg f declarations tbranch in
    (* Now get the pointer to the current builder position for later
       insertion of the unconditional branch statement*)
    let ptr_then_bb = insertion_block builder in
    (* Now build the Else block *)
    let else_bb = append_block context "fbranch" the_function in
    (* Now position the builder to insert into the else block*)
    let () = position_at_end else_bb builder in
    (* Now actually build the else branch until the end-node*)
    let () = codegen_cfg f declarations fbranch in
    (* Now get the pointer to the current builder position for later
       insertion of the unconditional branch statement*)
    let ptr_else_bb = insertion_block builder in
    (* Now we need to build the continue part using the endnode in the
       enode reference*)
    (match !enode with
      | Endnode (_,en,_) -> (* OK !! *)
	(* First we need to insert the continue block *)
	let cont_bb = append_block context "cond_cont" the_function in
	(* Now we need to add all the branch statements *)
	(* Adding the top most branch statment at start_bb *)
	let () = position_at_end start_bb builder in 
	let _ = build_cond_br cond_val then_bb else_bb builder in
	(* Now build the unconditional jump from then branch to cont*)
	let () = position_at_end ptr_then_bb builder in
	let _ = build_br cont_bb builder in
	(* Now build the unconditional jump from the else branch to cont *)
	let () = position_at_end ptr_else_bb builder in
	let _ = build_br cont_bb builder in
	(* This is the position that will be passed onto the parent
	   nodes *)
	let () = position_at_end cont_bb builder in
	let _ = insertion_block builder in
	(* Now set enode to point to the next cfg node *)
	enode := en
      | _ -> raise (Internal_compiler_error "If stmt does not return its endnode "))

let rec decompile_filter_params = function
  | Squarenode (stmt,cfg) -> 
    (match stmt with 
      | VarDecl (x,lc) -> x
      | _ -> raise (Internal_compiler_error ("Inputs/Outputs not of type VarDecl!!"))) :: decompile_filter_params cfg
  | Empty -> []
  | _ -> raise (Internal_compiler_error "Inputs/Outputs not declared in a square node!!")

(* For now only the name of the function will be present here *)
let codegen_prototype name ins outs =
  (* First we need to get the type of the inputs *)
  (* IMP: Inputs are always by copy and hence, can never be ptr type*)
  let intypes = List.map (fun x -> ((get_llvm_primitive_type (get_typed_symbol_lc x) (get_exact_typed_type x)) context)) ins in
  (* IMP: The outputs are always by alloca, and hence are always of ptr type*)
  let outtypes = List.map (fun x -> pointer_type ((get_llvm_primitive_type (get_typed_symbol_lc x) (get_exact_typed_type x)) context)) outs in
  (* Build the function prototype *)
  let ft = function_type (void_type context) (Array.of_list (intypes@outtypes)) in
  let f = match (lookup_function name the_module) with
    | Some _ -> raise (Error ("Function: " ^ name ^ "multiply defined"))
    | None -> declare_function name ft the_module in
  (* For clarity we will set the name of the parameters as well, this is not necessary though!!*)
  let args = Array.of_list (List.map (fun x -> get_typed_symbol x) (ins@outs)) in
  let () = Array.iteri (fun i a -> set_value_name (args.(i)) a) (params f) in f

let codegen_input_params the_function declarations inputs = 
  let () = IFDEF DEBUG THEN print_endline ("In codegen_input_params params length: " ^ (string_of_int (Array.length (params the_function)))) ELSE () ENDIF in
  let input_params = Array.sub (params the_function) 0 (Array.length inputs) in
  Array.iteri
    (fun i ai ->
      (match inputs.(i) with
	| SimTypedSymbol (x,y,lc) as s -> 
	  let datatype = get_llvm_primitive_type lc x in
	  let alloca = create_entry_block_alloca the_function (get_symbol y) (datatype context) in
	  (* Store the incoming value into the alloc structure *)
	  let _ = build_store ai alloca builder in
	  (* Add the var decl to the declarations list *)
	  add_to_declarations s alloca declarations
	| ComTypedSymbol (_,_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ "complex types currently not supported")))) input_params 
    
(* Just add the output params to the declarations, because they are pointer type*)
let codegen_output_params the_function declarations input_size outputs = 
  let () = IFDEF DEBUG THEN print_endline ("In codegen_output_params params length: " ^ (string_of_int (Array.length (params the_function)))) ELSE () ENDIF in
  let () = IFDEF DEBUG THEN print_endline ("Input_size: " ^ (string_of_int input_size)) ELSE () ENDIF in
  let all_params = (params the_function) in
  let output_params = Array.sub all_params input_size ((Array.length all_params) - input_size) in
  Array.iteri (fun i ai ->
    (match outputs.(i) with
      | SimTypedSymbol _ as s -> add_to_declarations s ai declarations
      | ComTypedSymbol (_,_,lc) -> raise (Error ((Reporting.get_line_and_column lc) ^ "complex types currently not supported")))) output_params

let llvm_topnode = function
  | Topnode (fcall,name,_,cfg_list) -> 
    let func_name = name ^ (string_of_int !func_name_counter) in
    (*Increment the func_name counter *)
    let () = func_name_counter := !func_name_counter + 1 in
    (* Added the new function name to the facll_name hashtbl *)
    let () = Hashtbl.add fcall_names name func_name in 
    let () = IFDEF DEBUG THEN print_endline (string_of_int (List.length cfg_list)) ELSE () ENDIF in
    (* You need to build the function prototype here *)
    let inputs = decompile_filter_params (List.nth cfg_list 0) in
    let outputs = decompile_filter_params (List.nth cfg_list 1) in
    let the_function = codegen_prototype func_name inputs outputs in
    (* You need to build the function body here using the prototype you got before*)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    (try
       (* Now make the inputs as allocas, what happens to the outputs
	  the outputs are just put directly into declarations, because
	  they are already alloc pointer_types *)
       let declarations = ref [] in
       let () = codegen_input_params the_function declarations (Array.of_list inputs) in 
       let () = codegen_output_params the_function declarations (List.length inputs) (Array.of_list outputs) in 
       let () = codegen_cfg the_function declarations (List.nth cfg_list 2) in
       let _ = build_ret_void builder in
       (* Validate that the function is correct *)
       Llvm_analysis.assert_valid_function the_function;
     (* Do all the optimizations on the function *)
     let _ = PassManager.run_function the_function the_fpm in
     (* Run the jit *)
     if name = "main" then
       (* Try loading the memory buffer file *)
       let membuf = MemoryBuffer.of_file "/tmp/print.bc" in
       (* Now read the bit code in *)
       let m2 = Llvm_bitreader.get_module context membuf in 
       (* Now add the module to the execution engine, just before we start executing*)
       let () = Llvm_executionengine.ExecutionEngine.add_module m2 exec_engine in
       let _ = Llvm_executionengine.ExecutionEngine.run_function_as_main the_function [||] [||] exec_engine in ()
     else ()
     with
       | e -> delete_function the_function; raise e)

  | Null -> raise (Internal_compiler_error "Hit a Null fcall expression while producing llvm IR")

let rec llvm_filter_node = function
  | Filternode (topnode, ll) -> 
    let () = List.iter llvm_filter_node ll in 
    let () = llvm_topnode topnode in
    (match topnode with Topnode(_,n,_,_) -> 
      if n = "main" then
	let () = dump_module the_module in
	if Llvm_bitwriter.write_bitcode_file the_module "output.ll" then ()
	else raise (Error ("Could not write the llvm module to output.ll file"))
      else ())

