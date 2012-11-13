module Kernel =
(* This is the loop outliner: essential when targeting the CUDA backend!! *)
(* Since this is targeted purely as CUDA kernels only the inner loops will be vectorized!! *)
(* This is no good for OpenMP, where we really need to parallelize outer loops! *)
(* The outer loop parallelizer should be moved to the llvm or a level just before it!! *)
(* There will be annotations produced which give hints to the next stage
   for parallelizing outer loops *)

(* This module is used by the myLlvm_cuda.ml file *)

(* What does vectorized code become on ptx?? *)

struct 

  open Language
  open Language
  open Vectorization

  exception Internal_compiler_error of string

  module List = Batteries.List
  module Hashtbl = Batteries.Hashtbl
  let nfilters = ref []
  let counter = ref 0

  (* CUDA requirements all blocks for the kernel should be equally *)
  (*    shaped. This means that we need to make sure that if we need more *)
  (*    than one block then we equally divide the number of threads in to *)
  (*    two or more blocks!!. But, does that mean we cannot use all the *)
  (*    threads inside a single block and say just one thread outside the *)
  (*    block??  *)

  (* FIXME: The below solution is currently not implemented and is a
     super optimization strategy, which I will not be doing right now!!
     strategy, which I will not be doing right now!!
     Yes this is true: So, it is better that may be we just use 1024
     threads and if there are "say" 2 more threads left then they should
     be rather run on the CPU!! *)

  (* maximum size of any dimension is 65535!! Be careful with this!! *)

  (* FIXME: Currently given the dimension of the loop and accesses we
     will just keep it as is. This should be determined by the
     auto-tuner*)

  let pindices = ref []
  let plimits = ref []
  let vars_used = ref []
  let vars_used_out = ref []
  let fmap = Hashtbl.create 10

  let get_symbol = function
    | Symbol (x,_) -> x

  let get_addressed_symbol = function
    | AddressedSymbol (x,_,_,_) -> get_symbol x

  let get_vec_symbol = function
    | VecAddress (x,_,_,_) -> (get_symbol x)

  let get_typed_symbol = function
    | SimTypedSymbol (_,x,_) -> get_symbol x
    | ComTypedSymbol (_,x,_) -> get_addressed_symbol x

  let add_to_pindices limits ll = 
    List.map2 (fun r -> fun (vstart,vend,vstride) -> if (List.exists (fun x -> (get_symbol x) = (get_symbol r)) !pindices) then () 
      else (pindices := !pindices@[r]; plimits := !plimits@[(Convert.get_access_size vstart vend vstride)];)) ll limits

  let add_used_var_out symbol_table x =
    if (List.exists (fun y -> (get_typed_symbol y) = x) symbol_table) then
      if (List.exists (fun y -> (get_typed_symbol y) = x) !vars_used_out) then () else vars_used := (List.find (fun y -> (get_typed_symbol y)=x)symbol_table)::!vars_used_out
    else if (List.exists (fun y -> (get_symbol y) = x) !pindices) then ()
    else raise (Internal_compiler_error ("Cannot find the symbol: " ^ x))

  let add_used_var symbol_table x =
    if (List.exists (fun y -> (get_typed_symbol y) = x) symbol_table) then
      if (List.exists (fun y -> (get_typed_symbol y) = x) !vars_used) then () else vars_used := (List.find (fun y -> (get_typed_symbol y)=x)symbol_table)::!vars_used
    else if (List.exists (fun y -> (get_symbol y) = x) !pindices) then ()
    else raise (Internal_compiler_error ("Cannot find the symbol: " ^ x))

  let rec get_used_vars = function
    | Plus (x,y,_) | Minus (x,y,_) | Times (x,y,_) | Div (x,y,_)
    | Pow (x,y,_) | Mod (x,y,_) | Rshift (x,y,_) | Lshift (x,y,_) -> (get_used_vars x) @ (get_used_vars y)
    | Abs (x,_) | Opposite (x,_) | Brackets (x,_) | Cast (_,x,_) -> (get_used_vars x)
    | Const _ | Constvector _ -> []
    | VarRef (x,_) -> [(get_symbol x)]
    | AddrRef (x,_) -> 
      (get_addressed_symbol x) ::
	(let d = (match x with AddressedSymbol (_,_,(x::[]),_) -> x) in 
	 (match d with BracDim x -> List.flatten (List.map (fun x -> get_used_vars (match x with DimSpecExpr x -> x))x)))
    | VecRef (_,x,_) -> (get_vec_symbol x) :: 
      (let d = (match x with VecAddress (_,_,x,_) -> x) in 
       List.flatten (List.map get_used_vars d))
    | _ as s -> raise (Internal_compiler_error ("Got a wrong simple expression in index: " ^ (Dot.dot_simpleexpr s)))

  let get_vars_used_l symbol_table = function
    | AllAddressedSymbol x ->
      let () = add_used_var_out symbol_table (get_addressed_symbol x) in
      let () = List.iter (add_used_var symbol_table)
	(let d = (match x with AddressedSymbol (_,_,(x::[]),_) -> x) in 
	 (match d with BracDim x -> List.flatten (List.map (fun x -> get_used_vars (match x with DimSpecExpr x -> x)) x))) in ()
    | AllSymbol x -> add_used_var_out symbol_table (get_symbol x)
    | AllTypedSymbol x -> add_used_var_out symbol_table (get_typed_symbol x)
    | AllVecSymbol (_,x) -> 
      let () = add_used_var_out symbol_table (get_vec_symbol x) in
      let () = List.iter (add_used_var symbol_table) 
	(let d = (match x with VecAddress (_,_,x,_) -> x) in 
	 List.flatten (List.map get_used_vars d)) in ()

  let get_vars_used_r symbol_table = function
    (* These should still be allowed in CUDA *)
    | FCall _ -> raise (Internal_compiler_error "Erroneously got a function call in the kernel!!")
    | SimExpr x -> List.iter (add_used_var symbol_table) (get_used_vars x)

  let build_kernel_body indices limits symbol_table = function
    | Assign (x,y,lc,_) as s -> 
      (* Now make the kernel with generic block and thread ids *)
      (* Add the indices to the pindices *)
      add_to_pindices limits indices;
      (* Now get all the vars other than indices being used in here *)
      (List.iter (get_vars_used_l symbol_table) x);
      (get_vars_used_r symbol_table y); 
    | Noop -> ()
    | _ as s -> raise (Internal_compiler_error (("Got erroneously: ") ^ (Dot.dot_stmt s)))

  let rec build_kernel indices limits symbol_table = function
    | Par (x,y,z,lc) ->
      let (vstart,vend,vstride) = Convert.get_par_bounds y in
      let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					       ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
      build_kernel (indices @ [x])  (limits@[(vstart,vend,vstride)]) symbol_table z
    | For _ -> raise (Internal_compiler_error "Got a wrong statement even after collapsing loops, should never happen!!")
    | Block (x,lc) -> List.iter (build_kernel indices limits symbol_table) x
    | _ as s -> build_kernel_body indices limits symbol_table s


  let rec stmts_without_pars stmts = function
    | Par (_,_,x,_) -> stmts_without_pars stmts x
    | Block (x,_) -> List.iter (stmts_without_pars stmts) x
    | For _ -> raise (Internal_compiler_error "Got a wrong statement even after collapsing loops, should never happen!!")
    | _ as s -> stmts := !stmts@[s]

  (* This function actually builds the kernel for CUDA *)
  let build_the_kernel (Par(x,_,z,lc)) = 
    if List.length !pindices > 3 then 
      raise (Internal_compiler_error 
	       "Currently we do not optimize more than 3D loop structures!!");
    (* Need to include the new indices in *)
    let stmts = List.flatten (List.mapi (fun i x ->
      let vv = (match i with 0 -> "x" | 1 -> "y" | 2 -> "z" 
	| _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "More than 3D loop in CUDA not allowed"))) in
      let sym1 = Symbol(((get_symbol x)^"ctaid"^(string_of_int i)),lc) in
      let sym2 = Symbol(((get_symbol x)^"ntid"^(string_of_int i)),lc) in
      let sym3 = Symbol(((get_symbol x)^"tid"^(string_of_int i)),lc) in
      let assign_l1 = AllTypedSymbol (SimTypedSymbol(DataTypes.Int32s,sym1,lc)) in
      let fcall1 = Call (Symbol(("@llvm.nvvm.read.ptx.sreg.ctaid."^vv),lc),[],lc) in
      let assign_r = FCall (fcall1,true) in
      let st1 = Assign ([assign_l1],assign_r,lc,Some NVVM_Internal) in
      let assign_l2 = AllTypedSymbol (SimTypedSymbol(DataTypes.Int32s,sym2,lc)) in
      let fcall2 = Call (Symbol(("@llvm.nvvm.read.ptx.sreg.ntid."^vv),lc),[],lc) in
      let assign_r2 = FCall (fcall2,true) in
      let st2 = Assign ([assign_l2],assign_r2,lc,Some NVVM_Internal) in
      let assign_l3 = AllTypedSymbol (SimTypedSymbol(DataTypes.Int32s,sym3,lc)) in
      let fcall3 = Call (Symbol(("@llvm.nvvm.read.ptx.sreg.tid."^vv),lc),[],lc) in
      let assign_r3 = FCall (fcall3,true) in
      let st3 = Assign ([assign_l3],assign_r3,lc,Some NVVM_Internal) in

      (* Now make the main multiplication and addition to get the index
	 for the statements *)
      let mul_assign = AllTypedSymbol 
	(SimTypedSymbol(DataTypes.Int32s,
			Symbol(((get_symbol x)^"mul"^(string_of_int i)),lc),lc)) in
      let mul_r = SimExpr (Times (VarRef (sym1, lc), VarRef (sym2, lc),lc)) in
      let sym4 = AllTypedSymbol(SimTypedSymbol (DataTypes.Int32s,x,lc)) in
      let assign = Assign ([sym4],mul_r,lc,None) in [st1;st2;st3;assign])!pindices) in
    (* Now just get the statements without the par loops!! *)
    let nstmts = ref [] in
    stmts_without_pars nstmts z;
    let block = Block ((stmts @ !nstmts),lc) in
    Filter ((Symbol (("__kernel__" ^ (string_of_int !counter)),lc)),!vars_used,!vars_used_out,block,Some (NVVM {ll= !plimits; outs= []; ins = []}))

  let get_call_symbol_names lc x = CallSymbolArgument (Symbol (get_typed_symbol x,lc))
  let get_assign_symbol_names lc x = AllSymbol (Symbol (get_typed_symbol x,lc))

  let build_intrinsic_call lc =
    let sym = Symbol (("__kernel__"^(string_of_int !counter)),lc) in
    let fcall = FCall (Call(sym,(List.map (get_call_symbol_names lc) !vars_used),lc),true) in
    Assign (List.map (get_assign_symbol_names lc) !vars_used_out,fcall,lc,Some (NVVM {ll = !plimits; outs= !vars_used_out; ins= !vars_used}))

  let rec process_filter_stmts symbol_table = function
    | Par (x,y,z,lc) as s -> 
      (* Try collapsing and build the vectorized code if you are
	 successful then that means we can also build the kernel, else we
	 cannot build the kernel and we continue *)
      (try
	 (if SafeToConvert.process_par s then
	     let (_,_,vstride) = Convert.get_par_bounds y in
	     if vstride <> 1 then raise (Internal_compiler_error "Currently we do not support CUDAing non stride 1 kernels!!");
	     (* Should I call convert instead?? GPUs are too different
		from CPUs to be sure of this *)
	     let _ = LoopCollapse.internal_convert !symbol_table s in 
	     (* Build the CUDA kernel *)
	     let (vstart,vend,vstride) = Convert.get_par_bounds y in
	     let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
						      ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
	     pindices := [];
	     plimits := [];
	     vars_used := [];
	     vars_used_out := [];
	     build_kernel [x]  [(vstart,vend,vstride)] !symbol_table z;
	     (* Now make the call to the damn thig via a 'C' intrinsic. This
		intrinsic will then specialize the block and thread_nums *)
	     (* We should also provide some special constraints we make the
		programmer assert for now*)
	     counter := !counter + 1;
	     let ret = build_intrinsic_call lc in
	     (* Now we actually build the kernel *)
	     let kernel = build_the_kernel s in
	     (* Add it to the nfilters list *)
	     nfilters := !nfilters @ [FCFG.Node (ret,kernel,None,[])];
	     (* Return an intrinsic function call *)
	     ret else s)
	  with
	    | _ -> s)
    | For (x,y,z,lc) -> For (x,y,(process_filter_stmts symbol_table z),lc)
    | Block (x,lc) -> Block ((List.map (process_filter_stmts symbol_table) x),lc)
    | VarDecl (x,_) as s -> symbol_table := !symbol_table@[x]; s
    | Split (x,lc) -> Split ((process_filter_stmts symbol_table x),lc)
    | CaseDef (x,lc) -> CaseDef (process_case symbol_table x,lc)
    | _ as s -> s

  and process_case symbol_table = function
    | Case (ll,o,lc) -> Case ((List.map (process_clause symbol_table) ll), (process_otherwise symbol_table o), lc)
  and process_clause symbol_table = function
    | Clause (r,s,lc) -> Clause (r, (process_filter_stmts symbol_table s), lc)
  and process_otherwise symbol_table = function
    | Otherwise (s,lc) -> Otherwise ((process_filter_stmts symbol_table s),lc)

  let proces_filter = function
    | Filter (x,y,z,st,sp) as s -> 
      let ret = Filter (x,y,z, (process_filter_stmts (ref (y@z)) st),sp) in 
      let () = Hashtbl.add fmap s !nfilters in ret
					    
  let rec outline = function
    | FCFG.Node (e,f,r,fl) -> 
      nfilters := [];
      let ff = proces_filter f in
      FCFG.Node (e,ff,r,(List.map outline fl)@(Hashtbl.find fmap f))

  let process node = 
    let () = IFDEF DEBUG THEN print_endline "entered loop_out process" ELSE () ENDIF in
    let () = Hashtbl.clear fmap in
    nfilters := [];
    counter := 0;
    let ret = outline node in ret
end 
