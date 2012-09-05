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
(* This is the module pass line, which does link time opimizations *)
let the_mpm = PassManager.create ()
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
let () = add_memcpy_opt the_fpm
let () = add_constant_propagation the_fpm
(* Add sparse conditional constant propogation *)
let () = add_sccp the_fpm

(* Now make the loop optimizations *)
let () = add_loop_unroll the_fpm

(* Initialize the pass manager *)
let _ =  PassManager.initialize the_fpm

(* Putting the jit in*)
(* let _ = Llvm_executionengine.initialize_native_target () *)
(* let exec_engine =  *)
(*   try  *)
(*     Llvm_executionengine.ExecutionEngine.create_jit the_module 2 *)
(*   with | s -> print_endline "Could not create jit"; raise s *)
(* let () = Llvm_target.TargetData.add (Llvm_executionengine.ExecutionEngine.target_data exec_engine) the_fpm *)

let target_data = 
Llvm_target.TargetData.create
"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:128-s0:64:64-f80:128:128-n8:16:32:64"
(* Set the target triple *)
let () = set_target_triple "x86_64-apple-darwin10.0.0" the_module 
let () = set_data_layout
"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:128-s0:64:64-f80:128:128-n8:16:32:64" 
  the_module

let fcall_names = Hashtbl.create 10
let func_name_counter = ref 1

let enode = ref Empty

type arg =
  | BLOCK
  | LOOP
  | CASE

(* The user specified modules *)
let user_modules = ref []

let get_symbol = function
  | Symbol (x,_) -> x
let get_vec_symbol = function
  | VecAddress (x,_,_) -> get_symbol x
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

let get_symbol_lc = function
  | Symbol(_,lc) -> lc
let get_addressed_symbol_lc = function
  | AddressedSymbol(_,_,_,lc) -> lc
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

let create_entry_block_alloca_array f vn t size = 
  let builder = builder_at context (instr_begin (entry_block f)) in
  let ret = build_alloca t vn builder in
  let amd = mdstring context "align 128" in
  (* let () = set_metadata ret 0 amd in *) ret
  

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

let get_brac_dims = function
  | DimSpecExpr x ->
    (match x with
      | Const (x,y,_) ->
	if (not (List.exists (fun r -> x = r) DataTypes.integral)) || ((int_of_string y) < 0) then
	  let () = IFDEF DEBUG THEN print_endline ("Dim is: " ^ y) ELSE () ENDIF in
	  raise (Error ("LLVM IR: Dimensions of array not of type unsignedIntegral, it is: " ^ y))
	else y
      | _ -> raise (Internal_compiler_error "LLVM IR hits non-const inside dimensions after constant folding!!"))

let get_dims = function
  | BracDim x -> List.map get_brac_dims x

let get_first_order_type = function
  | SimTypedSymbol (x,_,_) -> (x,[])
    (* Remember we have gotten rid of all the angledim <> lists, so that
       we only have [] and [,,,][] expressions with drop semantics
       left *)
  | ComTypedSymbol (x,y,_) -> 
      (* let () = IFDEF DEBUG THEN print_endline "get_first_order_type: got complex typed symbol" ELSE () ENDIF in *)
    (x, List.flatten (List.map get_dims (match y with AddressedSymbol (_,_,y,_) -> y)))

let rec drop todrop counter dims = function
  | h::t -> if not (List.exists (fun x -> x = counter) todrop) then dims := h :: !dims; drop todrop (counter+1) dims t
  | [] -> ()

let get declarations s = 
  try
    List.find (fun x -> (match x with (x,y) -> (get_typed_symbol x) = s)) declarations
  with
    | Not_found -> raise (Internal_compiler_error ("Variable: " ^ s ^ " could not be found"))

let exists declarations s = 
  List.exists (fun x -> (match x with (x,y) -> (get_typed_symbol x)) = s) declarations

let add_to_declarations s alloca declarations = 
  if (exists !declarations (get_typed_symbol s)) then 
    (* let () = print_curr_line_num !curr_lnum in *)
    raise (Error (((Reporting.get_line_and_column (get_typed_symbol_lc s)) ^ "Symbol "^ (get_typed_symbol s)) ^ " multiply defined" ))
  else declarations := (s,alloca) :: !declarations


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

let rec get_simexpr_type declarations = function
  | Plus (x,_,_) | Minus (x,_,_) | Pow (x,_,_) | Lshift(x,_,_) | Rshift(x,_,_)
  | Div (x,_,_) | Times (x,_,_) | Mod (x,_,_) -> get_simexpr_type declarations x
  | Opposite (x,_) -> get_simexpr_type declarations x
  | Const (x,_,lc) -> get_data_types lc x
  | Cast (x,_,lc) -> get_data_types lc x
  | Brackets (x,_) -> get_simexpr_type declarations x
  | VarRef (x,_) -> let (x,_) = get declarations (get_symbol x) in get_typed_type x
  | AddrRef (x,_) -> let (x,_) = get declarations (get_addressed_symbol x) in get_typed_type x
  | VecRef (x,_) -> let (x,_) = get declarations (get_vec_symbol x) in get_typed_type x
  | Constvector (x,_,lc) -> get_data_types lc x
  | _ -> raise (Internal_compiler_error ("Unsupported simple expr"))

let rec get_exact_simexpr_type declarations = function
  | Plus (x,_,_) | Minus (x,_,_) | Pow (x,_,_) | Lshift(x,_,_) | Rshift(x,_,_)
  | Div (x,_,_) | Times (x,_,_) | Mod (x,_,_) -> get_exact_simexpr_type declarations x
  | Opposite (x,_) -> get_exact_simexpr_type declarations x
  | Const (x,_,_) -> x
  | Cast (x,_,_) -> x
  | Brackets (x,_) -> get_exact_simexpr_type declarations x
  | VarRef (x,_) -> let (x,_) = get declarations (get_symbol x) in get_exact_typed_type x
  | AddrRef (x,lc) -> let (x,_) = get declarations (get_addressed_symbol x) in get_exact_typed_type x
  | VecRef (x,lc) -> let (x,_) = get declarations (get_vec_symbol x) in get_exact_typed_type x
  | Constvector (x,_,_) -> x
  | _ as s -> raise (Internal_compiler_error (("Unsupported simple expr") ^ Dot.dot_simpleexpr s))

let derefence_pointer pointer =
  match classify_type (type_of pointer) with
    | TypeKind.Pointer -> build_load pointer "dereference_pointer" builder
    | _ -> pointer

let rec codegen_simexpr declarations = function
  | Plus (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_add (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "addtemp" builder
      | "float" -> build_fadd (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "addftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Minus (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_sub (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "subtemp" builder
      | "float" -> build_fsub (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "subftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))
  | Times (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_mul (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "timestemp" builder
      | "float" -> build_fmul (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer(codegen_simexpr declarations y)) "timesftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Lshift (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_shl (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "shltemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Rshift (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_lshr (derefence_pointer (codegen_simexpr declarations x)) (derefence_pointer (codegen_simexpr declarations y)) "templshr" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  (* We need the exact type for building the division instruction *)
  | Div (x,y,lc) ->
    (match get_simexpr_type declarations x with
      | "int" -> 
	let ret = build_sdiv (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer(codegen_simexpr declarations y)) "divtemp" builder in
	let () = IFDEF DEBUG THEN dump_value ret ELSE () ENDIF in
	(* Debugging --> get the type of the value *)
	let () = IFDEF DEBUG THEN
	  (match classify_type (type_of ret) with
	    | TypeKind.Float -> print_endline "Type is float"
	    | TypeKind.Integer -> print_endline "Type is int"
	    | TypeKind.Double -> print_endline "Type is double"
	    | _ -> print_endline "unknown type") ELSE () ENDIF in ret
      | "float" -> build_fdiv (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer(codegen_simexpr declarations y)) "divftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Pow (x,y,lc) -> raise(Error((Reporting.get_line_and_column lc) ^ " LLVM does not support pow/wow internally :-("))

  | Mod (x,y,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_srem (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer(codegen_simexpr declarations y)) "remtemp" builder 
      | "float" ->build_frem (derefence_pointer(codegen_simexpr declarations x)) (derefence_pointer(codegen_simexpr declarations y)) "remftemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " wrong type!!")))

  | Opposite (x,lc) -> 
    (match get_simexpr_type declarations x with
      | "int" -> build_neg (derefence_pointer(codegen_simexpr declarations x)) "negtemp" builder
      | "float" -> build_fneg (derefence_pointer(codegen_simexpr declarations x)) "negftemp" builder
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
      | "sext" -> 
	let () = IFDEF DEBUG THEN print_endline "casting sext" ELSE () ENDIF in
	build_sext (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "sexttemp" builder
      | "trunc" -> 
	let () = IFDEF DEBUG THEN print_endline "casting trunc" ELSE () ENDIF in
	build_trunc (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "trunctemp" builder
      | "fpext" -> 
	let () = IFDEF DEBUG THEN print_endline "casting fpext" ELSE () ENDIF in
	build_fpext (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "fpexttemp" builder
      | "fptrunc" -> 
	let () = IFDEF DEBUG THEN print_endline "casting fptrunc" ELSE () ENDIF in
	build_fptrunc (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "fptrunctemp" builder
      | "sitofp" -> 
	let () = IFDEF DEBUG THEN print_endline "casting sitofp" ELSE () ENDIF in
	build_sitofp (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "sitofptemp" builder
      | "uitofp" -> 
	let () = IFDEF DEBUG THEN print_endline "casting uitofp" ELSE () ENDIF in
	build_uitofp (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "uitofptemp" builder
      | "fptosi" -> 
	let () = IFDEF DEBUG THEN print_endline "casting fptosi" ELSE () ENDIF in
	build_fptosi (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "fptositemp" builder
      | "fptoui" -> 
	let () = IFDEF DEBUG THEN print_endline "casting fptoui" ELSE () ENDIF in
	build_fptoui (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "fptouitemp" builder
      | "zext" -> 
	let () = IFDEF DEBUG THEN print_endline "casting zext" ELSE () ENDIF in
	build_zext (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "zext" builder
      | "=" -> 
	let () = IFDEF DEBUG THEN print_endline "casting zext" ELSE () ENDIF in
	build_zext (derefence_pointer(codegen_simexpr declarations y)) ((get_llvm_primitive_type lc ctype) context) "zext" builder
      | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^ " got a wrong type!! " ^ s)))
  | Brackets (x,_) -> codegen_simexpr declarations x

  | VarRef (x,lc) -> 
    (* Now this can also be an array type, in which case the pointer
       needs to be sent back, not the value!! *)
    (match (match (get declarations (get_symbol x)) with | (x,_) -> x) with 
      | ComTypedSymbol _ ->
	(* In this case we need to send back the pointer to the array!! *)
	let findex = const_int (Llvm_target.intptr_type target_data) 0 in
	build_in_bounds_gep (match get declarations (get_symbol x) with | (_,x)->x) [|findex;findex|] "tempgep" builder
      | _ -> build_load (match get declarations (get_symbol x) with | (_,x) -> x) (get_symbol x) builder)

  | AddrRef (x,lc) as s ->
    (* We need to make getelementptr instructions here do them in order not the reverse *)
    let array_llvaue = (match get declarations (get_addressed_symbol x) with | (_,x) -> x) in
    let _ = (match get declarations (get_addressed_symbol x) with | (x,_) -> match x with ComTypedSymbol (x,_,_) ->x
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc)^" not of a complex type"))) in
    (* Now we need to build the indices array !! *)
    (* We know that the very first index should always be 0, because it
       should point to the address of the pointer pointing to the array
       itself*)
    (* Need to get the intptr_type from the target data *)
    let findex = const_int (i32_type context) 0 in
    (* let findex = const_int (get_llvm_primitive_type lc (get_exact_simexpr_type declarations s) context) 0 in *)
    (* Now we need to get the codegen for the index variables *)
    (* We should just use the indices that we need to drop and use them instead of this one!!*)
    (* let indices = get_to_drop x in *)
    let indices = (match x with AddressedSymbol (_,_,[BracDim x],lc) -> let x = List.map (fun (DimSpecExpr x)->x) x in 
								     List.map (fun x -> (derefence_pointer(codegen_simexpr declarations x))) x
      | _ -> raise (Internal_compiler_error "Wrong addressed symbol type")) in
    let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) indices ELSE () ENDIF in
    let itypes = (match x with AddressedSymbol (_,_,[BracDim x],lc) -> let x = List.map (fun (DimSpecExpr x)->x) x in 
								     List.map (fun x -> get_exact_simexpr_type declarations x) x
      | _ -> raise (Internal_compiler_error "Wrong addressed symbol type")) in
    let () = IFDEF DEBUG THEN dump_value array_llvaue ELSE () ENDIF in
    let ret = build_in_bounds_gep array_llvaue (Array.of_list [findex;findex]) "tempgep" builder in
    (* let ret = build_in_bounds_gep array_llvaue (Array.of_list (findex::indices)) "tempgep" builder in *)
    (* Then we need to sign extend the thing to ptr type *)
    let indices = List.map2 (fun x y ->
      (match DataTypes.cmp_datatype (x,DataTypes.Int64s) with
    	| "sext" -> build_sext y (Llvm_target.intptr_type target_data) "sexttemp" builder
    	| _ -> y)) itypes indices in
    let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) indices ELSE () ENDIF in
    (* We have to do this one after the other!! *)
    let ret = ref ret in
    let counter = ref (List.length indices) in
    let () = List.iter (fun x -> 
      (ret := build_in_bounds_gep !ret [|x|] "tempgep" builder;
       counter := !counter - 1 ;
       (* If we have more than one index then you need a 0,0 ret as well*)
       if !counter = 0 then ()
       else
	 (* Build the extra 0,0 address referencing *)
	 ret := build_in_bounds_gep !ret (Array.of_list [findex;findex]) "tempgep" builder)) indices in 
    !ret

  (* Generating the non constant vector *)
  | VecRef (x,lc) as s ->
  (* This is easy to handle

     1.) Convert all the non-colon-expressions to AddrRef type and dereference them by calling this same method!!
     2.) Next, the pointer that one gets back needs to be loaded, it will always be of type vector!! 
     3.) Then make the shuffle mask by calling the function in vectorization module
     4.) build the new shuffled vector by calling build_shuffle_vector, and pass on that value back!!

  *)
    
    let (name,ll) = (match x with VecAddress (x, ll,_) -> (x,ll)) in
    let dll = (match ll with | BracDim x -> List.filter (fun (DimSpecExpr x) -> (match x with ColonExpr _ -> false | _ -> true)) x) in
    let ce = (match ll with | BracDim x -> List.filter (fun (DimSpecExpr x) -> (match x with ColonExpr _ -> true | _ -> false)) x) in
    if List.length ce <> 1 then raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " more than one colon expression in a vector statemetn"));
    let ce = (match (List.hd ce) with | DimSpecExpr x -> x) in
    (* Now make the address symbol *)
    let vptr = 
      if dll <> [] then
	let aptr = AddrRef (AddressedSymbol (name,[],[BracDim dll],lc),lc) in
	(* Now load the vector with the returned pointer *)
	codegen_simexpr declarations aptr
      else match get declarations (get_vec_symbol x) with | (_,x) -> x in
    (* Now build the shuffle mask *)
    let (s,e,st,lc) = (match ce with 
      | ColonExpr (x,y,z,lc) as s -> let (s,e,st) = Vectorization.Convert.get_par_bounds s in (s,e,st,lc)
      | _ as ettt -> raise (Internal_compiler_error ("Vector index not of type colon expr" ^ Dot.dot_simpleexpr ettt))) in
    let mask = Vectorization.Convert.build_shuffle_mask s e st lc in
    let mask = Array.map (fun x -> Const (DataTypes.Int32, (string_of_int x),lc)) mask in
    let mask = const_vector (Array.map (codegen_simexpr declarations) mask) in
    let dv = match get declarations (get_vec_symbol x) with | (x,_) -> x in
    let (vtyp,lc) = (match dv with | ComTypedSymbol (x,_,lc) -> (x,lc) | _ -> raise (Internal_compiler_error " Vector not of correct type")) in
    let uptl = (undef (vector_type ((get_llvm_primitive_type lc vtyp) context) (vector_size (type_of (derefence_pointer vptr))))) in
    build_shufflevector (derefence_pointer vptr) uptl mask "shuff_vec" builder
      

  (* Generating the const vector *)
  | Constvector (dt, sa, lc) -> 
    let () = IFDEF DEBUG THEN print_endline ("Vector size: " ^ (string_of_int (Array.length sa))) ELSE () ENDIF in
    let ca = Array.map (fun x -> 
      (match x with 
	| Const (x,y,lc) as s -> codegen_simexpr declarations s
	| _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got a const vector with non constants in them")))) sa in
    const_vector ca


  | TStar | TStarStar -> raise (Error "TStar and TStartStar currently not supported in LLVM IR generation")
  | _ -> raise (Internal_compiler_error "Bogus simple expression hit while producing LLVM IR" )

(* TODO: Make the very first type always a vector type instead of an array type !! *)
let get_exact_addressed_symbol_llvm_type ptype = function
  | AddressedSymbol (name,_,[BracDim x],lc) -> 
    (* We only have dimspecexpr list*)
    (* We know that all dimspecexpr are just const types *)
    (* We need to reverse the list and keep on building the array_type recursively*)
    let x = List.map (fun (DimSpecExpr x) -> x) x in
    let atype = ref ptype in
    let slist = List.rev x in
    let () = List.iteri (fun i x -> 
      (match x with | Const (_,value,_) -> 
	(if i = 0 then
	    atype := vector_type !atype (int_of_string value)
	 else
	    atype := array_type !atype (int_of_string value))
	| _ -> raise (Internal_compiler_error((Reporting.get_line_and_column lc)^ " array length not of type const")))) slist 
    in !atype
  | _ -> raise (Internal_compiler_error "get_addressed_symbol_llvm_type went wrong")


let get_addressed_symbol_llvm_type ptype = function
  | AddressedSymbol (name,_,[BracDim x],lc) -> 
    (* We only have dimspecexpr list*)
    (* We know that all dimspecexpr are just const types *)
    (* We need to reverse the list and keep on building the array_type recursively*)
    let x = List.map (fun (DimSpecExpr x) -> x) x in
    let atype = ref ptype in
    let slist = List.rev (List.tl x) in
    let () = List.iter (fun x -> (match x with | Const (_,value,_) -> atype := array_type !atype (int_of_string value)
      | _ -> raise (Internal_compiler_error((Reporting.get_line_and_column lc)^ " array length not of type const")))) slist 
    in (!atype, List.hd x)
  | _ -> raise (Internal_compiler_error "get_addressed_symbol_llvm_type went wrong")

let codegen_typedsymbol f declarations = function
  | SimTypedSymbol (x,y,lc) as s -> 
    let datatype = get_llvm_primitive_type lc x in
    let alloca = create_entry_block_alloca f (get_symbol y) (datatype context) in
    (* Add the var decl to the declarations list *)
    add_to_declarations s alloca declarations
  (* Sun Aug 26 16:23:30 IST 2012 : 
  
     TODO:
     
     Change to have the most internal ones as vector types, always!!
  *)
  | ComTypedSymbol (x,y,lc) as s -> (* raise (Error ((Reporting.get_line_and_column lc) ^ "complex types currently not supported")) *)
    (* Produce and array type alloca *)
    (* This is my primitive data-type *)
    let datatype = get_llvm_primitive_type lc x in
    (* Now we need to find if this primitive data-type needs to be extended
       to arrays or matrices inductively*)
    let (datatype1,size) = get_addressed_symbol_llvm_type (datatype context) y in
    let datatype = get_exact_addressed_symbol_llvm_type (datatype context) y in
    (* Data type may be a primitmive type if it is a vector type *)
    (* Now alloca the array on the stack *)
    let size = (match size with Const _ -> codegen_simexpr !declarations size 
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "Arrays first dim not of type const"))) in
    let alloca = create_entry_block_alloca_array f (get_addressed_symbol y) datatype size in
    add_to_declarations s alloca declarations
    
(* We need to know the exact type of every expression *)
let rec codegen_relexpr declarations = function
  | LessThan (x,y,lc) -> 
    let lhs = derefence_pointer(codegen_simexpr declarations x) in
    let rhs = derefence_pointer(codegen_simexpr declarations y) in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Slt lhs rhs "Slttemp" builder
      | "float" -> build_fcmp Fcmp.Olt lhs rhs "Olttemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | LessThanEqual (x,y,lc) -> 
    let lhs = derefence_pointer(codegen_simexpr declarations x) in
    let rhs = derefence_pointer(codegen_simexpr declarations y) in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sle lhs rhs "Sletemp" builder
      | "float" -> build_fcmp Fcmp.Ole lhs rhs "Oletemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | GreaterThan (x,y,lc) -> 
    let lhs = derefence_pointer(codegen_simexpr declarations x) in
    let rhs = derefence_pointer(codegen_simexpr declarations y) in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sgt lhs rhs "Sgttemp" builder
      | "float" -> build_fcmp Fcmp.Ogt lhs rhs "Ogttemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | GreaterThanEqual (x,y,lc) -> 
    let lhs = derefence_pointer(codegen_simexpr declarations x) in
    let rhs = derefence_pointer(codegen_simexpr declarations y) in
    (* Get the type *)
    (match get_simexpr_type declarations x with
      | "int" -> build_icmp Icmp.Sge lhs rhs "Sgetemp" builder
      | "float" -> build_fcmp Fcmp.Oge lhs rhs "Ogetemp" builder
      | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "unknown type")))
  | EqualTo (x,y,lc) -> 
    let lhs = derefence_pointer(codegen_simexpr declarations x) in
    let rhs = derefence_pointer(codegen_simexpr declarations y) in
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

let get_num_indices = function
  | AddressedSymbol (_,_,x,_) -> match (List.hd x) with BracDim x -> List.length x

let codegen_callargs defmore lc declarations = function
  | CallSymbolArgument x ->
    let alloca = (match get declarations (get_symbol x) with | (_,x) -> x) in 
    (match (match get declarations (get_symbol x) with | (x,_) -> x) with
      | ComTypedSymbol (x,y,lc) as s -> 
	(* In this case we need to send back the pointer to the array!! *)
	(* let findex = const_int ((get_llvm_primitive_type lc x) context) 0 in *)
	let findex = const_int (i32_type context) 0 in
	(* We send the element pointer pointing to the first element if
	   it is a vector type*)
	(* We need to get the pointer to the very first element of this thing*)
	if not defmore then
	  let indices = Array.make 1 findex in
	  (* Make a list of 0 of size primitive array type *)
	  build_in_bounds_gep alloca indices "tempgep" builder
	else
	  let x = y in
	  let indices = (match x with AddressedSymbol (_,_,[BracDim x],lc) -> let x = List.map (fun (DimSpecExpr x)->x) x in 
									      List.map (fun x -> codegen_simexpr declarations (Const (DataTypes.Int32s,"0",lc))) x
	    | _ -> raise (Internal_compiler_error "Wrong addressed symbol type")) in
	  let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) indices ELSE () ENDIF in
	  let itypes = (match x with AddressedSymbol (_,_,[BracDim x],lc) -> let x = List.map (fun (DimSpecExpr x)->x) x in 
									     List.map (fun x -> get_exact_simexpr_type declarations x) x
	    | _ -> raise (Internal_compiler_error "Wrong addressed symbol type")) in
	  let () = IFDEF DEBUG THEN dump_value alloca ELSE () ENDIF in
	  let ret = build_in_bounds_gep alloca (Array.of_list [findex;findex]) "tempgep" builder in
	  (* Then we need to sign extend the thing to ptr type *)
	  let indices = List.map2 (fun x y ->
	    (match DataTypes.cmp_datatype (x,DataTypes.Int64s) with
    	      | "sext" -> build_sext y (Llvm_target.intptr_type target_data) "sexttemp" builder
    	      | _ -> y)) itypes indices in
	  let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) indices ELSE () ENDIF in
	  (* We have to do this one after the other!! *)
	  let ret = ref ret in
	  let counter = ref (List.length indices) in
	  let () = List.iter (fun x -> 
	    (ret := build_in_bounds_gep !ret [|x|] "tempgep" builder;
	     counter := !counter - 1 ;
	     (* If we have more than one index then you need a 0,0 ret as well*)
	     if !counter = 0 then ()
	     else
	       (* Build the extra 0,0 address referencing *)
	       ret := build_in_bounds_gep !ret (Array.of_list [findex;findex]) "tempgep" builder)) indices in 
	  !ret
      | _ -> build_load alloca (get_symbol x) builder)
  | CallAddrressedArgument x -> 
    (* If we are dropping all the elements then we need to send a load
       rather than the pointer itself, because that is what the function
       expects!*)
    let dec_type = get_first_order_type (match (get declarations (get_addressed_symbol x)) with (x,_) -> x) in
    let to_drop = (get_to_drop x) in
    if ((List.length to_drop) > (List.length (snd (dec_type)))) then
      raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "LLVM IR: dropping more dimensions then the size of the polytope"))
    else
      let actual_dims = ref [] in
      (drop to_drop 0 actual_dims (snd dec_type));
      let pointer = codegen_simexpr declarations (AddrRef (x,(get_addressed_symbol_lc x))) in
      if (List.length !actual_dims) = 0 then build_load pointer "pointercall" builder
      else pointer

let codegen_fcall lc declarations = function
  | FCall (x,e) ->
    (* First lookup the name of the function *)
    let (name,args,lc) =
      if not e then
	(match x with
	  | Call (name,y,lc) -> 
	    (* lookup name in fcall_names Hashtbl *)
	    try
	      ((Hashtbl.find fcall_names (get_symbol name)),y,lc)
	    with 
	      | Not_found -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ get_symbol name ^ " not found in hashtbl for fcall names")))
      else 
	(match x with 
	  | Call (name,y,lc) -> (get_symbol name, y, lc)) in
    (* Now start building the llvm build_call *)
    let () = IFDEF DEBUG THEN print_endline ("Found function: " ^ name) ELSE () ENDIF in
    if not e then
      let callee = (match lookup_function name the_module with
	| Some callee -> callee
	| None -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ name ^ " function not in llvm module"))) in
      let bargs = List.map (fun x -> codegen_callargs e lc declarations x) args in (callee,bargs)
    else 
      (* Move through all the user loaded modules and get the callee and the bargs *)
      let callee_l = List.filter (fun x -> match lookup_function name x with | Some _ -> true | None -> false) !user_modules in
      if callee_l = [] then raise (Error((Reporting.get_line_and_column lc) ^ "No function named: " ^ name ^ " found in llvm modules"))
      else if List.length callee_l <> 1 then raise (Error((Reporting.get_line_and_column lc) ^ "More than one module with same function declaration"))
      else 
	let callee = match (lookup_function name (List.hd callee_l)) with | Some x -> x
	  | None -> raise (Error((Reporting.get_line_and_column lc) ^ "No function named: " ^ name ^ " found in llvm modules")) in
	let () = IFDEF DEBUG THEN dump_value callee ELSE () ENDIF in
	(* Now declare the same named function with external linkage in this module *)
	(* get the param types from the params *)
	let () = IFDEF DEBUG THEN print_endline ("getting " ^ name ^ " param types") ELSE () ENDIF in
	let param_types = Array.map (fun x -> type_of x) (params callee) in
	(* Set the alignment for the parameters *)
	let () = Array.iter (fun x -> set_param_alignment x 32) (params callee) in
	let callee_attrs = (function_attr callee) in
	let callee = (match lookup_function name the_module with
	  | Some x -> x
	  | None -> 
	    let () = IFDEF DEBUG THEN print_endline ("building extern " ^ name ^ " declaration") ELSE () ENDIF in
	    let eft = function_type (void_type context) param_types in
	    let reft = declare_function name eft the_module in
	    (* Also set the attributes for this function *)
	    let () = List.iter (fun x -> add_function_attr reft x) callee_attrs in
	    reft) in
	(* Set the linkage to external type *)
	let () = set_linkage Linkage.External callee in
	let () = IFDEF DEBUG THEN print_endline (match linkage callee with | Linkage.External -> "External" | _ -> "Some other linkage type") ELSE () ENDIF in
	let bargs = List.map (fun x -> codegen_callargs e lc declarations x) args in
	let () = IFDEF DEBUG THEN print_endline "found the external function delcaration" ELSE () ENDIF in
	(callee,bargs)
  | SimExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " ended up with SimExpr when it shouldn't"))

let codegen_asssimexpr lc declarations = function
  | SimExpr x ->
    let r = codegen_simexpr declarations x in
    let () = IFDEF DEBUG THEN print_endline "dumping the rvalue simexpr: " ELSE () ENDIF in
    let () = IFDEF DEBUG THEN dump_value r ELSE () ENDIF in r
  | FCall _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " ended up with FCall when it shouldn't"))

let rec strech v s1 to_inc vtyp declaration = 
  let () = IFDEF DEBUG THEN print_endline ("Streching by : " ^ (string_of_int to_inc)) ELSE () ENDIF in
  if (to_inc < 0) then raise (Internal_compiler_error "Cannot make the result vector shorter than the current length!!")
  else if s1 = to_inc then v
  else 
    (* Now first build a 0 sized vector *)
    let to_inca = const_vector (Array.init s1 (fun i -> codegen_simexpr declaration (Const(vtyp, "0", (0,0))))) in
    let () = IFDEF DEBUG THEN dump_value to_inca ELSE () ENDIF in
    (* Now use the shuffle instruction to increase the size of this vector to s2*)
    let msize = if (2*s1 < to_inc) then (2*s1) else to_inc in
    let mask = Array.init msize (fun i -> codegen_simexpr declaration (Const(DataTypes.Int32s, (string_of_int i), (0,0)))) in
    let mask = const_vector mask in
    let () = IFDEF DEBUG THEN dump_value mask ELSE () ENDIF in
    let v = build_shufflevector v to_inca mask "strech" builder in
    let () = IFDEF DEBUG THEN dump_value v ELSE () ENDIF in
    strech v (vector_size (type_of v)) to_inc vtyp declaration

let codegen_stmt f declarations = function
  | Assign (ll, expr, lc) -> 
    (* codegen the rhs *)
    let () = IFDEF DEBUG THEN print_endline "assigning" ELSE () ENDIF in
    (* add the declaration to the *)
    (match expr with
      | SimExpr t ->
	let rval = codegen_asssimexpr lc !declarations expr in
	let rval = (match t with
	  (* Need to load the value from the pointer just calculated*)
	  | AddrRef _ -> build_load rval "loadgep" builder
	  (* Even if this is a vector type it should be alright to just send the vector loaded back!!*)
	  | _ -> rval) in
	(* Any kind of vectorization code can only affect this place!! *)
	List.iter (fun x ->
	  (match x with
	    | AllTypedSymbol x ->
	      let () = codegen_typedsymbol f declarations x in
	      let (_,alloca) = get !declarations (get_typed_symbol x) in
	      let _ = build_store rval alloca builder in ()
	    | AllSymbol x -> 
	      (* lookup that the symbol is in the declarations list and get it *)
	      let (x,alloca) = get !declarations (get_symbol x) in
	      (match x with
		| ComTypedSymbol _ ->
		  raise (Error((Reporting.get_line_and_column (get_typed_symbol_lc x))^ " direct assignment to arrays currently not supported :-("))
		| _ -> let _ = build_store rval alloca builder in ())
	    | AllAddressedSymbol x -> 
	      (* This is similar to simple-expr codegen for AddrRef*)
	      let () = IFDEF DEBUG THEN print_endline "codegen for lvalue of type addrref" ELSE () ENDIF in
	      let tempaddref = AddrRef(x,(get_addressed_symbol_lc x)) in
	      let assaddrval = codegen_simexpr !declarations tempaddref in
	      let () = IFDEF DEBUG THEN print_endline "Dumping the lvalue of type addr: "  ELSE () ENDIF in
	      let () = IFDEF DEBUG THEN dump_value assaddrval ELSE () ENDIF in
	      let _ = build_store rval assaddrval builder in ()

	    | AllVecSymbol x -> 
	      (* FIXME: Make this go away later on!! *)
	      let (name,ll) = (match x with VecAddress (x, ll,_) -> (x,ll)) in
	      let dll = (match ll with | BracDim x -> List.filter (fun (DimSpecExpr x) -> (match x with ColonExpr _ -> false | _ -> true)) x) in
	      let ce = (match ll with | BracDim x -> List.filter (fun (DimSpecExpr x) -> (match x with ColonExpr _ -> true | _ -> false)) x) in
	      if List.length ce <> 1 then raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " more than one colon expression in a vector statement"));
	      let ce = (match (List.hd ce) with | DimSpecExpr x -> x) in
	      (* Now make the address symbol *)
	      let vptr = 
		if dll <> [] then
		  let aptr = AddrRef (AddressedSymbol (name,[],[BracDim dll],lc),lc) in
		(* Now load the vector with the returned pointer *)
		codegen_simexpr !declarations aptr
		else match get !declarations (get_vec_symbol x) with | (_,x) -> x in
	      let () = IFDEF DEBUG THEN dump_value vptr ELSE () ENDIF in
	      (* Now build the shuffle mask *)
	      let (s,e,st,lc) = (match ce with
		| ColonExpr (x,y,z,lc) as s -> let (s,e,st) = Vectorization.Convert.get_par_bounds s in (s,e,st,lc)
		| _ as ettt -> raise (Internal_compiler_error ("Vector index not of type colon expr" ^ Dot.dot_simpleexpr ettt))) in
	      let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int s)
						       ^ "(end:) " ^ (string_of_int e) ^ "(stride:) " ^ (string_of_int st)) ELSE () ENDIF in
	      let mask = Vectorization.Convert.build_inverse_shuffle_mask s e st lc in
	      let mask1 = Array.map (fun x -> Const (DataTypes.Int32, (string_of_int x),lc)) mask in
	      (* Now again shuffle with the complete length of the vector, but put rval first and then put *)
	      let _ =
		if Array.length mask1 <> 0 then 
		  let mask = const_vector (Array.map (codegen_simexpr !declarations) mask1) in
		  let () = IFDEF DEBUG THEN dump_value mask ELSE () ENDIF in
		  let vtpt = (derefence_pointer vptr) in
		  let () = IFDEF DEBUG THEN dump_value vtpt ELSE () ENDIF in
		  let dv = match get !declarations (get_vec_symbol x) with | (x,_) -> x in
		  let (vtyp,lc) = (match dv with | ComTypedSymbol (x,_,lc) -> (x,lc) | _ -> raise (Internal_compiler_error " Vector not of correct type")) in
		  let uptl = (undef (vector_type ((get_llvm_primitive_type lc vtyp) context) (vector_size (type_of vtpt)))) in
		  let () = IFDEF DEBUG THEN dump_value uptl ELSE () ENDIF in
		  let inver_s = build_shufflevector vtpt uptl mask "shuff_vec" builder in
		  let () = IFDEF DEBUG THEN dump_value inver_s ELSE () ENDIF in
		  (* Build the permutation mask for shuffling things in!! *)
		  let tot = Vectorization.Convert.build_counter_mask s e in
		  let first = Vectorization.Convert.build_inverse_shuffle_mask s e st lc in
		  let second = Vectorization.Convert.build_shuffle_mask s e st lc in
		  let mask = Vectorization.Convert.build_permute_mask tot first second lc in
		  let mask = Array.map (fun x -> Const (DataTypes.Int32, (string_of_int x),lc)) mask in
		  let mask = const_vector (Array.map (codegen_simexpr !declarations) mask) in
		  let () = IFDEF DEBUG THEN dump_value mask ELSE () ENDIF in
		  (* It is possible that the rval or the inver_s, either
		     one of them is shorter than the other.  We want to
		     make them of equal size and hence, we should pad
		     the shorter one with zeros. This won't really have
		     any affect on the result, because the shuffle mask
		     will correctly pick up the required elements from
		     the vectors!! *)
		  let () = IFDEF DEBUG THEN print_endline 
		    ((string_of_int (vector_size (type_of rval))) ^ " = "  ^ (string_of_int (vector_size (type_of inver_s)))) ELSE () ENDIF in
		  let rval = 
		    if (vector_size (type_of rval)) < (vector_size (type_of inver_s)) then
		      strech rval (vector_size (type_of rval)) (vector_size (type_of inver_s)) vtyp !declarations 
		    else 
		      let () = IFDEF DEBUG THEN print_endline "return old rval" ELSE () ENDIF in
		      rval in
		  let inver_s = if (vector_size (type_of inver_s)) < (vector_size (type_of rval)) then
		      strech inver_s (vector_size (type_of inver_s)) (vector_size (type_of rval))  vtyp !declarations else inver_s in
		  let resptr = build_shufflevector inver_s rval mask "shuff_vec" builder in
		  build_store resptr vptr builder
		else 
		  (* Just store the whole rval in here!! *)
		  build_store rval vptr builder in ())) ll

      | FCall (_,e) as s -> 
	let (callee,args) = codegen_fcall lc !declarations s in
	let () = IFDEF DEBUG THEN (List.iter (fun x -> dump_value x) args) ELSE () ENDIF in
	(* The assignments types are all alloca types just get the actual alloca *)
	let oargs = List.map (fun x ->
	  (match x with
	    | AllTypedSymbol x -> 
	      let () = codegen_typedsymbol f declarations x in
	      let (r,alloca) = get !declarations (get_typed_symbol x) in 
	      (* We need to pass in the very first element of the pointer *)
	      (match r with
		| SimTypedSymbol _ -> alloca
		| ComTypedSymbol(x,y,lc) as s -> 
		  let findex = const_int (Llvm_target.intptr_type target_data) 0 in
		  (* let findex = const_int ((get_llvm_primitive_type lc x)context) 0 in *)
		  build_in_bounds_gep alloca (Array.of_list [findex]) "otempgep" builder)
	    | AllSymbol x -> 
	      (* lookup that the symbol is in the declarations list and get it *)
	      let (r,alloca) = get !declarations (get_symbol x) in
	      (match r with
		| SimTypedSymbol _ ->  alloca
		| ComTypedSymbol(x,y,lc) -> 
		  (* let findex = const_int ((get_llvm_primitive_type lc x)context) 0 in *)
		  let findex = const_int (Llvm_target.intptr_type target_data) 0 in
		  build_in_bounds_gep alloca (Array.of_list [findex]) "otempgep" builder)
	    | AllAddressedSymbol x -> 
	      (* rval itself might be pointers as well *)
	      (* These will also be pointer types ofcourse !! *)
	      (* This is similar to simple-expr codegen for AddrRef*)
	      let tempaddref = AddrRef(x,(get_addressed_symbol_lc x)) in
	      codegen_simexpr !declarations tempaddref)) ll in
	(* Now make the call *)
	let () = IFDEF DEBUG THEN List.iter (fun x -> dump_value x) oargs ELSE () ENDIF in
	let d =  build_call callee (Array.of_list (args@oargs)) "" builder in
	let () = IFDEF DEBUG THEN dump_value d ELSE () ENDIF in ())
  | VarDecl (x,lc) -> codegen_typedsymbol f declarations x
  | Noop -> ()
  | _ as s -> 
    let () = IFDEF DEBUG THEN (print_endline (Dot.dot_stmt s)) ELSE () ENDIF in
    raise (Internal_compiler_error "Hit a bogus stmt while compiling to llvm")

let get_vars = function
  | VarDecl (x,_)  -> [x]
  | Assign (x,_,_) -> List.flatten (List.map (fun x -> (match x with AllTypedSymbol x -> [x] | _ -> [])) x)
    (* | Block x -> List.flatten (List.map (fun x -> get_vars x) x) *)
  | _ -> []

let rec get_dvars = function
  | Block (x,_) -> List.flatten (List.map (fun x -> get_vars x) x)
  | For (x,y,z,lc) | Par (x,y,z,lc) -> 
    let sym = (SimTypedSymbol (DataTypes.Int32s, x,lc)) in
    sym :: (get_vars z)
  | CaseDef (case,_) -> 
    let (clause_list,other) = (match case with Case (x,y,_) -> (x,y)) in
    let clause_stmt_list = List.map (fun x -> (match x with Clause (_,x,_) -> x)) clause_list in
    let other_stmt = (match other with Otherwise (x,_) -> x) in
    (List.flatten (List.map (fun x -> get_vars x) clause_stmt_list)) @ (get_vars other_stmt)
  | _ -> []
    
let rec remove_from_declarations declarations dvars = 
  List.iter (fun r -> declarations := List.filter (fun (x,_) -> x <> r) !declarations) dvars

(* Generate the body of the function block *)
let rec codegen_cfg arg f declarations = function
  | Empty -> let () = IFDEF DEBUG THEN print_endline "hit an empty node" ELSE () ENDIF in ()
  | Startnode (stmt,x) -> 
    (match stmt with
      | CaseDef _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a case startnode") ELSE () ENDIF in
	let () = codegen_cfg CASE f declarations x in
	(* Now we need to continue with the enode!!*)
	codegen_cfg arg f declarations !enode
      | For _ | Par _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a loop startnode") ELSE () ENDIF in
	let () = codegen_cfg LOOP f declarations x in
	(* Now we need to continue with the enode!!*)
	codegen_cfg arg f declarations !enode
      | _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a block startnode") ELSE () ENDIF in
	codegen_cfg arg f declarations x)
  | Squarenode (stmt,x) ->
    let () = IFDEF DEBUG THEN print_endline ("Hit a squarenode") ELSE () ENDIF in
    let () = codegen_stmt f declarations stmt in
    codegen_cfg arg f declarations x
  | Endnode (stmt,x,_) as s -> 
    (match stmt with
      | CaseDef _ | For _ | Par _ -> enode := s (* Set yourself up !! *)
      | _ -> 
	let () = IFDEF DEBUG THEN print_endline ("Hit a endnode") ELSE () ENDIF in
	(* Remove the dvars from the declarations *)
	let dvars = get_dvars stmt in
	let () = remove_from_declarations declarations dvars in
	codegen_cfg arg f declarations x)
  | Backnode x -> () (* We don't need to do anything here at all!! *)
  | Conditionalnode (relexpr,tbranch,fbranch) ->
    (match arg with
	| CASE -> 
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
	  let () = codegen_cfg arg f declarations tbranch in
	  (* Now get the pointer to the current builder position for later
	     insertion of the unconditional branch statement*)
	  let ptr_then_bb = insertion_block builder in
	  (* Now build the Else block *)
	  let else_bb = append_block context "fbranch" the_function in
	  (* Now position the builder to insert into the else block*)
	  let () = position_at_end else_bb builder in
	  (* Now actually build the else branch until the end-node*)
	  let () = codegen_cfg arg f declarations fbranch in
	  (* Now get the pointer to the current builder position for later
	     insertion of the unconditional branch statement*)
	  let ptr_else_bb = insertion_block builder in
	  (* Now we need to build the continue part using the endnode in the
	     enode reference*)
	  (match !enode with
	    | Endnode (stmt,en,_) -> (* OK !! *)
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
	      let dvars = get_dvars stmt in
	      let () = remove_from_declarations declarations dvars in
	      enode := en
	    | _ -> raise (Internal_compiler_error "If stmt does not return its endnode "))
	| LOOP -> 
	  (* First get the current basic block and the parent function *)
	  let start_bb = insertion_block builder in
	  (* This might be some other basic block*)
	  let the_function = block_parent start_bb in
	  (* Now append the then basic block to the start_bb*)
	  let loop = append_block context "loopbody" the_function in
	  (* Position the builder to append to the true branch block *)
	  let () = position_at_end loop builder in
	  (* Next build the true branch *)
	  let () = codegen_cfg arg f declarations tbranch in
	  (* First insert the conditional *)
	  let cond_val = codegen_relexpr !declarations relexpr in
	  (* Now get the pointer to the current builder position for later
	     insertion of the unconditional branch statement*)
	  let ptr_loop_body = insertion_block builder in
	  (* Now actually build the else branch until the end-node*)
	  (* Nothing should ever be really built at all!! *)
	  let () = codegen_cfg arg f declarations fbranch in
	  (* Now build the loop end *)
	  (match !enode with
	    | Endnode (stmt,en,_) ->
	      let loopend = append_block context "loopend" the_function in
	      (* Now build the conditional branch *)
	      let () = position_at_end ptr_loop_body builder in
	      let _ = build_cond_br cond_val loop loopend builder in
	      (* Now attach the branch instruction after start_bb*)
	      let () = position_at_end start_bb builder in 
	      let _ = build_br loop builder in
	      (* Now position the pointer so that stuff can be inserted
		 into loopend blocks *)
	      let () = position_at_end loopend builder in
	      let _ = insertion_block builder in 
	      (* Remove the declared vars from the endnode *)
	      let dvars = get_dvars stmt in
	      let () = remove_from_declarations declarations dvars in
	      enode := en
	    | _ -> raise (Internal_compiler_error ("LOOP did not get a Endnode")))
	| BLOCK -> raise (Internal_compiler_error ("Conditional node with BLOCK type")))

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
  (* IMP: Inputs are always by copy and hence, can never be ptr type, except in case of arrays being passed in as inputs!! *)
  (* First check if this is a complex type *)
  let intypes = List.map (fun x -> 
    (match x with 
      | ComTypedSymbol (x,y,lc) as s -> 
	(* This is a special case, it needs to be a pointer type!! *)
	(* Just do as you did for comtypedsymbol*)
	let datatype = get_llvm_primitive_type lc x in
	(* Now we need to find if this primitive data-type needs to be extended
	   to arrays or matrices inductively*)
	let datatype = get_exact_addressed_symbol_llvm_type (datatype context) y in
	pointer_type datatype
	(* Now make the pointer to this type *)
	(* pointer_type (get_llvm_primitive_type (get_typed_symbol_lc s) (get_exact_typed_type s) context) *)
      | SimTypedSymbol _ -> (get_llvm_primitive_type (get_typed_symbol_lc x) (get_exact_typed_type x) context))) ins in
  (* IMP: The outputs are always by alloca, and hence are always of ptr type*)
  let outtypes = List.map (fun x -> 
    match x with
      | ComTypedSymbol (x,y,lc) as s -> 
	(* (\* This is a special case, it needs to be a pointer type!! *\) *)
	(* (\* Just do as you did for comtypedsymbol*\) *)
	let datatype = get_llvm_primitive_type lc x in
	(* (\* Now we need to find if this primitive data-type needs to be extended *)
	(*    to arrays or matrices inductively*\) *)
	let datatype = get_exact_addressed_symbol_llvm_type (datatype context) y in
	pointer_type datatype
	(* pointer_type ((get_llvm_primitive_type (get_addressed_symbol_lc y) (get_exact_typed_type s)) context) *)
      | SimTypedSymbol _ -> pointer_type ((get_llvm_primitive_type (get_typed_symbol_lc x) (get_exact_typed_type x)) context)) outs in
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
	  let datatype = type_of ai in
	  let alloca = create_entry_block_alloca the_function (get_symbol y) datatype in
	  (* Store the incoming value into the alloc structure *)
	  let _ = build_store ai alloca builder in
	  (* Add the var decl to the declarations list *)
	  (* let () = add_param_attr ai Attribute.Noalias in *)
	  add_to_declarations s alloca declarations
	| ComTypedSymbol (x,y,lc) as s ->
	  (* Set the attributes as readonly *)
	  let () = add_param_attr ai Attribute.Byval in
	  let () = add_param_attr ai Attribute.Noalias in
	  (* let () = (match classify_type (type_of ai) with | TypeKind.Pointer -> add_param_attr ai  Attribute.Byval | _ -> ()) in *)
	  (* let datatype = type_of ai in *)
	  (* let alloca = create_entry_block_alloca the_function (get_addressed_symbol y) datatype in *)
	  (* Store the incoming value into the alloc structure *)
	  (* let _ = build_store ai alloca builder in *)
	  (* Add the var decl to the declarations list *)
	  (* add_to_declarations s alloca declarations *)
	  add_to_declarations s ai declarations)) input_params
    
(* Just add the output params to the declarations, because they are pointer type*)
let codegen_output_params the_function declarations input_size outputs = 
  let () = IFDEF DEBUG THEN print_endline ("In codegen_output_params params length: " ^ (string_of_int (Array.length (params the_function)))) ELSE () ENDIF in
  let () = IFDEF DEBUG THEN print_endline ("Input_size: " ^ (string_of_int input_size)) ELSE () ENDIF in
  let all_params = (params the_function) in
  let output_params = Array.sub all_params input_size ((Array.length all_params) - input_size) in
  Array.iteri (fun i ai ->
    (* TODO: Check if this works for array types !! *)
    (match outputs.(i) with
      | SimTypedSymbol _ as s -> add_to_declarations s ai declarations
      | ComTypedSymbol (x,y,lc) as s -> 
	(* In this case we need to make a pointer pointer by using alloca *)
	(* let datatype = type_of ai in *)
	(* let alloca = create_entry_block_alloca the_function (get_addressed_symbol y) datatype in *)
	(* (\* Store the incoming value into the alloc structure *\) *)
	(* let _ = build_store ai alloca builder in *)
	(* Add the var decl to the declarations list *)
	let () = add_param_attr ai Attribute.Noalias in
	add_to_declarations s ai declarations)) output_params

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
    let the_function = (if name = "main" then
      codegen_prototype name inputs outputs else codegen_prototype func_name inputs outputs) in
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
       let () = codegen_cfg BLOCK the_function declarations (List.nth cfg_list 2) in
       let _ = build_ret_void builder in
       (* Validate that the function is correct *)
       Llvm_analysis.assert_valid_function the_function;
     (* Do all the optimizations on the function *)
     let _ = PassManager.run_function the_function the_fpm in the_function
     with
       | e -> delete_function the_function; raise e)
  | Null -> raise (Internal_compiler_error "Hit a Null fcall expression while producing llvm IR")

let rec llvm_filter_node filename = function
  | Filternode (topnode, ll) -> 
    let () = List.iter (fun x -> llvm_filter_node filename x) ll in 
    let the_function = llvm_topnode topnode in
    (match topnode with Topnode(_,n,_,_) ->
      if n = "main" then
    	let () = dump_module the_module in
    	let () = if Llvm_bitwriter.write_bitcode_file the_module (filename^".bc") then ()
    	else raise (Error ("Could not write the llvm module to output.ll file")) in ()
      (* let _ = Llvm_executionengine.ExecutionEngine.run_function_as_main the_function [||] [||] exec_engine in () *)
      else ())

let compile modules filename cfg = 
  (* Set the modules that need to be loaded *)
  (* Try loading the memory buffer file *)
  (* Go through all the files and do it!!*)
  let membufs = List.map (fun x -> MemoryBuffer.of_file x) modules in
  (* Now read the bit code in *)
  let modules = List.map (fun x -> Llvm_bitreader.get_module context x) membufs in 
  (* let () = IFDEF DEBUG THEN List.iter (fun x -> dump_module x) modules ELSE () ENDIF in *)
  (* TODO: Optimize the modules just parsed *)
  (* let _ = List.map (fun x -> PassManager.run_module x the_mpm) modules in *)
  (* (\* Now add the module to the execution engine, just before we start executing*\) *)
  (* let () = List.iter(fun x -> Llvm_executionengine.ExecutionEngine.add_module x exec_engine) modules in *)
  (* let () = Llvm_target.TargetData.add (Llvm_executionengine.ExecutionEngine.target_data exec_engine) the_mpm in *)
  (* let () = IFDEF DEBUG THEN print_endline "***** PRINTING THE PRINT FUNCTION ******" ELSE () ENDIF in *)
  (* let () = IFDEF DEBUG THEN dump_value (match Llvm_executionengine.ExecutionEngine.find_function "print" exec_engine with Some x -> x  *)
  (*   | None -> raise (Internal_compiler_error ("Could not find function print, even after adding the module"))) ELSE () ENDIF in *)
  (* Set the user ssupported modules *)
  user_modules := modules;
  let () = llvm_filter_node filename cfg in ()
  (* (\* free the jit *\) *)
  (* Llvm_executionengine.ExecutionEngine.dispose exec_engine *)

