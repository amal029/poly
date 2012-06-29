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
let () = add_basic_alias_analysis the_fpm
let () = add_type_based_alias_analysis the_fpm
(* Memory to register promotion *)
let () = add_memory_to_register_promotion the_fpm
(* common sub-expresion elimination *)
let () = add_gvn the_fpm

(* Initialize the pass manager *)
let _ =  PassManager.initialize the_fpm

let fcall_names = Hashtbl.create 10
let func_name_counter = ref 1

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
    let () = add_to_declarations s alloca declarations in alloca
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

let codegen_expr lc declarations = function
  | SimExpr x -> 
    let r = codegen_simexpr declarations x in
    let () = IFDEF DEBUG THEN dump_value r ELSE () ENDIF in r
  | FCall _ -> raise (Error ((Reporting.get_line_and_column lc) ^ "function calls currently not supported"))

let codegen_stmt f declarations = function
  | Assign (ll, expr, lc) -> 
    (* codegen the rhs *)
    let () = IFDEF DEBUG THEN print_endline "assigning" ELSE () ENDIF in
    let rval = codegen_expr lc !declarations expr in
    (* add the declaration to the *)
    let _ = List.map (fun x -> 
    (match x with
      | AllTypedSymbol x -> 
	codegen_typedsymbol f declarations x
      | AllSymbol x ->
	(* lookup that the symbol is in the declarations list and get it *)
	let (_,alloca) = get !declarations (get_symbol x) in
	build_store rval alloca builder
      | AllAddressedSymbol _ -> raise (Error ((Reporting.get_line_and_column lc)^ "complex types currently not supported in llvm IR generation")))) ll in 
    rval
  | VarDecl (x,lc) -> codegen_typedsymbol f declarations x
  | _ -> raise (Internal_compiler_error "Hit a bogus stmt while compiling to llvm")

(* Generate the body of the function block *)
let rec codegen_cfg f declarations = function
  | Startnode (_,x) -> 
    let () = IFDEF DEBUG THEN print_endline ("Hit a startnode") ELSE () ENDIF in
    codegen_cfg f declarations x
  | Squarenode (stmt,x) ->
    let () = IFDEF DEBUG THEN print_endline ("Hit a squarenode") ELSE () ENDIF in
    let ret = codegen_stmt f declarations stmt in
    let r2 = codegen_cfg f declarations x in
    if is_null r2 then ret else r2
  | Endnode (_,x,_) -> 
    let () = IFDEF DEBUG THEN print_endline ("Hit a endnode") ELSE () ENDIF in
    codegen_cfg f declarations x
  | Backnode x -> raise (Error ("Loops currently not handeled in the LLVM backend"))
  | Conditionalnode _ -> raise (Error ("If and loops currently not handeled in the llvm backend"))
  | Empty -> 
    let () = IFDEF DEBUG THEN print_endline ("hit an empty node") ELSE () ENDIF in 
    const_null (i1_type context)
  (* For now only the name of the function will be present here *)
let build_prototype name declarations =
    (* Build the function prototype *)
  let ft = function_type (void_type context) [||] in
  let f = match (lookup_function name the_module) with
    | Some _ -> raise (Error ("Function: " ^ name ^ "multiply defined"))
    | None -> declare_function name ft the_module in
    (* Now set the param names *)
    (* The declared params internally using the declare_function to the
       actual args *)
    (*FIXME: currently we don't need to do any of this!*)
  f

let llvm_topnode = function
  | Topnode (fcall,name,_,cfg_list) -> 
    let func_name = name ^ (string_of_int !func_name_counter) in
      (*Increment the func_name counter *)
    let () = func_name_counter := !func_name_counter + 1 in
    (* Added the new function name to the facll_name hashtbl *)
    let () = Hashtbl.add fcall_names name func_name in 
      (* FIXME: You cannot take in any input or give out any output arguments *)
    let () = IFDEF DEBUG THEN print_endline (string_of_int (List.length cfg_list)) ELSE () ENDIF in
    (* You need to build the function prototype here *)
    let the_function = build_prototype func_name [] in
      (* You need to build the function body here using the prototype you got before*)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    (try
       let ret = codegen_cfg the_function (ref []) (List.nth cfg_list 2) in
       let _ = build_ret_void builder in
       (* Validate that the function is correct *)
       Llvm_analysis.assert_valid_function the_function;
       (* Do all the optimizations on the function *)
       let _ = PassManager.run_function the_function the_fpm in ()
     with
       | e -> delete_function the_function; raise e)

  | Null -> raise (Internal_compiler_error "Hit a Null fcall expression while producing llvm IR")

let rec llvm_filter_node = function
  | Filternode (topnode, ll) -> 
    let () = llvm_topnode topnode in
    let () = dump_module the_module in
    let () = List.iter llvm_filter_node ll in 
    if Llvm_bitwriter.write_bitcode_file the_module "output.ll" then ()
    else raise (Error ("Could not write the llvm module to output.ll file"))

