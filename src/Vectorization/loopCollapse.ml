(**

   Sun Sep  9 13:01:34 IST 2012
   
   Purpose: 
   
   Carries out outer loop collapsing and then vectorization
   
   Just use all the declarations and functions in Module convert in file
   vectorization.ml (Depends upon vectorization.ml)

*)

open Language
open Language
open Vectorization
open Convert

module Array = Batteries.Array
module List = Batteries.List
module Int = Batteries.Int
module Float = Batteries.Float

let my_transpose = ref false
let my_runtime_vec = ref false

let strech_looking_at_child size list = 
  Array.of_list (List.flatten (List.map (fun x -> Array.to_list (Array.init size (fun i -> x))) list))

let strech_looking_at_parent size list = 
  Array.of_list (List.flatten (Array.to_list (Array.init size (fun i -> list))))

let rec get_used_indices_and_access_sizes indices limits for_indices = function
  | Brackets (x,_) -> get_used_indices_and_access_sizes indices limits for_indices x
  | Cast (_,x,_) -> get_used_indices_and_access_sizes indices limits for_indices x
  | VarRef (x,lc) -> 
    if List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices then
      let (index,_) = List.findi (fun i t -> (get_symbol t) = (get_symbol x)) indices in
      let () = IFDEF DEBUG THEN print_endline ("Var ref index: " ^ (string_of_int index)) ELSE () ENDIF in
      let (vstart,vend,vstride) = List.nth limits index in
      (* This is your own repitition vector *)
      let my_size = get_access_size vstart vend vstride in
      [(x,my_size)]
    (* This might exist in the for loop bounds try to find it*)
    else
      List.filter (fun (i,y) -> (get_symbol x) = (get_symbol i)) for_indices
  | Const (_,_,lc)  -> [(Symbol ("Const",lc),0)]
  | _ -> []

let rec existsin indices = function
  | Brackets (x,_) -> existsin indices x
  | Cast (_,x,_) -> existsin indices x
  | VarRef (x,lc) -> 
    List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices
  | Const _ -> true
  | _ -> false

let transpose_varref indices limits for_indices used index dropped yexpr to_rep = function
  | VarRef (x,lc) ->
    let () = IFDEF DEBUG THEN print_endline "Inside print" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN List.iter (fun (DimSpecExpr x) -> print_endline (Dot.dot_simpleexpr x)) !yexpr ELSE () ENDIF in
    if not (List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices) then
      (* We need to replace this index with the one that is in par *)
      (* Only if it exists in the used symbol list *)
      (try 
	 let (x,size) = List.find (fun (i,t) -> (get_symbol x) = (get_symbol i)) used in
	 (* Now try to get another index which is in the par indices from
	    the rest of the dropped list *)
	 let (id,xd) = List.findi (fun i (DimSpecExpr x) -> existsin indices x) dropped in
	 (* Check if they both have equal access sizes *)
	 (* Now we can replace the two indices id and index with each others expressions *)
	 let dused = (get_used_indices_and_access_sizes indices limits for_indices) (match xd with DimSpecExpr x -> x)  in
	 let dused_size = List.map (fun (_,x) -> x) dused in
	 let dused_size = 
	   if (List.length dused_size) <> 1 then raise (Internal_compiler_error "")
	   else List.hd dused_size in
	 if dused_size = 0 || dused_size = size then
	   (* We can replace the two expressions with each other in yexpr *)
	   yexpr := List.mapi (fun i (DimSpecExpr x) -> 
	     if i = (id+index+1) then 
	       let () = IFDEF DEBUG THEN print_endline ("i is: " ^ (string_of_int i) ^ " id is: " ^ (string_of_int id) ^ " expr is: " ^ (Dot.dot_simpleexpr to_rep)) 
		 ELSE () ENDIF in
	       DimSpecExpr to_rep 
	     else if i = index then 
	       let () = IFDEF DEBUG THEN print_endline ("i is: " ^ (string_of_int i) ^ " index is: " ^ (string_of_int index) ^ " expr is: " ^ (Dot.dot_simpleexpr (match xd with DimSpecExpr xd -> xd))) 
		 ELSE () ENDIF in
	       xd 
	     else DimSpecExpr x) !yexpr;
       with
	 | _ -> ())

  | _ -> raise (Internal_compiler_error "")

let transpose_indices indices limits for_indices used index ll yexpr = function
  | Brackets (x,lc) as s ->
    (match x with 
      | VarRef _ as x -> 
	transpose_varref indices limits for_indices used index ll yexpr s x
      | _ -> ())
  | Cast (d,x,lc) as s -> 
    (match x with 
      | VarRef _ as x -> 
	transpose_varref indices limits for_indices used index ll yexpr s x
      | _ -> ())
  | VarRef _ as s -> 
    transpose_varref indices limits for_indices used index ll  yexpr s s
  | _ -> ()
  
let rec multiply_indices multiplicand = function
  | Plus (x,y,lc) -> Plus (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Minus (x,y,lc) -> Minus (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Times (x,y,lc) -> Times (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Div (x,y,lc) -> Div (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Rshift (x,y,lc) -> Rshift (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Lshift (x,y,lc) -> Lshift (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Mod (x,y,lc) -> Mod (multiply_indices multiplicand x,multiply_indices multiplicand y,lc)
  | Brackets (x,lc) -> Brackets (multiply_indices multiplicand x, lc)
  | Opposite (x,lc) -> Opposite (multiply_indices multiplicand x, lc)
  | Constvector (o,dt,array,lc) -> Constvector (o,dt,(Array.map (fun x -> (match x with | Const (a,x,lc)-> 
    let res = ((int_of_string x) * multiplicand) in Const (a,(string_of_int res),lc))) array),lc)
  | Vector (dt,expr,ii,lc) as s ->
    if !my_runtime_vec then 
      let cvec = Constvector (None,dt,(Array.init ii (fun i -> Const (dt,(string_of_int multiplicand),lc))),lc) in
      Times (s,cvec,lc)
      (* Vector (dt,((match expr with VarRef _ as s -> Times (s,Const(DataTypes.Int32s,(string_of_int multiplicand),lc),lc))),ii,lc) *)
    else raise (Internal_compiler_error "Got a Vector type index without the -floop-runtime-vector option")
  | _ as s -> raise (Internal_compiler_error ("Currently index type: " ^ (Dot.dot_simpleexpr s) ^ " not supported"))

let rec build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc = function
  | Plus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Plus (l,r,lc)

  | Minus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Minus (l,r,lc)

  | Times (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Times (l,r,lc)

  | Div (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Div (l,r,lc)

  | Pow (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Pow (l,r,lc)

  | Mod (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Mod (l,r,lc)

  | Rshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Rshift (l,r,lc)

  | Lshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    Lshift (l,r,lc)

  | Brackets (x,lc) -> Brackets (build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x, lc)

  (* FIXME: (Improve) Cannot cast need to make a vector type cast!! *)
  | Cast (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc y in
    (* If this is a const matrix then it is easy to cast else for now give an error type!! *)
    (match l with 
      | Constvector (r,_,x1,lc) -> Constvector(r,x,x1,lc)
      | _ -> raise (Internal_compiler_error "Cannot cast non const vector types yet!! "))

  | Opposite (x,lc) -> Opposite (build_collaped_vec_simexpr_1 index_arg indices limits symbol_table for_indices lc x, lc)

  | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))

  | Const (x,y,lc) ->
    (* Special case expand the vector to the required size and give a const vector back!! *)
    (* let size = (vend - vstart + 1)/vstride in  *)
    let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
    let () = IFDEF DEBUG THEN print_endline ("Array size: " ^ (string_of_int size)) ELSE () ENDIF in
    Constvector (None,x,(Array.init size (fun i -> Const (x,y,lc))),lc)

  | AddrRef (x,lc) -> 
    let vecad = build_collapsed_addressed_symbol_vec indices limits symbol_table for_indices lc x in
    let (access_sizes,dims) = (match vecad with | VecAddress (_,x,y,_) -> (x,y)) in
    let shuffle_mask = List.mapi (fun i x -> try (i,(calculate_shuffle_mask access_sizes lc x)) with | Ignore _ -> (i,x) | _ -> raise (Internal_compiler_error "")) dims in
    let shuffle_mask = List.filter (fun (_,x) -> filter_shm x) shuffle_mask in
    (* let shuffle_mask = List.filter (fun (_,x) -> (match x with Constvector _ -> true | _ -> false)) shuffle_mask in *)
    (* First we need to get the integer variables from these *)
    let indices_converted = List.map (fun (x,_) -> x) shuffle_mask in
    let symbol = try get symbol_table (get_symbol (match x with | AddressedSymbol (x,_,_,_) -> x)) with 
      | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let (tot_size,to_calc) = (match symbol with ComTypedSymbol (_,x,_) -> 
      (match x with | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with | BracDim x -> 
	(* First we need to filter out the ones, which will be converted *)
	let to_calc = List.mapi (fun i (DimSpecExpr x) -> 
	  if (List.exists (fun x -> x = i) indices_converted) then x 
	  else TStar) x in
	let to_calc = List.filter (fun x -> (match x with TStar -> false | _ -> true)) to_calc in
	(((List.fold_right (fun (Const (_,x,_)) y -> ((int_of_string x)) * y) to_calc 1)-1),to_calc)))
      | _ -> raise (Internal_compiler_error "")) in
    (* First get the parent indices *)
    let to_calc = List.map (fun (Const(_,x,_)) -> (int_of_string x)) to_calc in
    let shuffle_mask = List.mapi (fun i x -> 
      let to_calc = List.drop (i+1) to_calc in
      let multiplicand = List.fold_right (fun x y -> x * y) to_calc 1 in
      multiply_indices multiplicand x) (List.map (fun (_,x)->x) shuffle_mask) in
    (* Add all the constantvectors in the compiler itself the non
       constants need to be added at runtime. The result is either a
       constantvector or a simple expression list *)
    (* Filtering out the constant vectors *)
    let smm = List.filter (fun x -> (match x with Constvector _ -> true | _ -> false)) shuffle_mask in
    let shuffle_mask = 
      if not (List.is_empty smm) then
	let smmv = List.filter (fun x -> (match x with Constvector _ -> false | _ -> true)) shuffle_mask in
	let smma = List.map (fun (Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x)) x) smm in
	let (a,b,c) = (match List.hd (smm) with | Constvector (a,b,_,lv) -> (a,b,lv)) in
	let rsm = Array.init (Array.length (List.hd smma)) (fun i -> 0) in
	let () = List.iter (fun x -> Array.iteri (fun i x -> rsm.(i) <- rsm.(i) + x) x) smma in
	(* The ordering of the shuffle mask should not matter, bcecause it
	   is an addition operation which is assocative and commutative *)
	Constvector (a,b,Array.map (fun x -> Const (DataTypes.Int32s,(string_of_int x),lc)) rsm,c) :: smmv  
      else shuffle_mask in
    VecRef (shuffle_mask,vecad,lc)

  | VarRef (x,lc) as s ->
    (* Induction variables can only be of type Int32s (for now)!! *)
    if List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices then
      let (index,_) = List.findi (fun i t -> (get_symbol t) = (get_symbol x)) indices in
      let () = IFDEF DEBUG THEN print_endline ("Var ref index: " ^ (string_of_int index)) ELSE () ENDIF in
      let (vstart,vend,vstride) = List.nth limits index in
      (* This is your own repitition vector *)
      let my_size = get_access_size vstart vend vstride in
      (* Get the repitition vectors above you *)
      let () = IFDEF DEBUG THEN print_endline (get_symbol x) ELSE () ENDIF in
      let (plist, clist) = List.split_at (index+1) limits in
      let () = IFDEF DEBUG THEN print_endline ("Parent list size before take: " ^ (string_of_int (List.length plist))) ELSE () ENDIF in
      let plist = List.take index plist in
      let () = IFDEF DEBUG THEN print_endline ("Parent list size: " ^ (string_of_int (List.length plist))) ELSE () ENDIF in
      let () = IFDEF DEBUG THEN print_endline ("Child list size: " ^ (string_of_int (List.length clist))) ELSE () ENDIF in
      let parent_size = 
	if plist <> [] then
	  List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) plist 1 
	else 0 in
      let child_size =
	if clist <> [] then
	  List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x)  clist 1 
	else 0 in
      (* Build your own shuffle mask *)
      let counter = ref vstart in
      let ar = Array.init my_size (fun i -> let ret = !counter in counter := !counter + vstride; ret) in
      (* Now we need to strech this constant array by first looking at child and then looking at parent *)
      let ar = if clist <> [] then strech_looking_at_child child_size (Array.to_list ar) else ar in
      let ar = if plist <> [] then strech_looking_at_parent parent_size (Array.to_list ar) else ar in
      let ar = Array.map (fun x -> Const(DataTypes.Int32s, (string_of_int x), lc)) ar in
      Constvector (Some (get_symbol x), DataTypes.Int32s, ar, lc)
    (* Now we start taking care of scalars, which are tops as well *)
    (* This means we have called this function for converting and index
       type, which is a non loop induction variable in this loop. For
       now we assume that this is an invariant variable in this
       loop. Else the person has given a wring par loop!! *)
    else if index_arg && not !my_runtime_vec then
      let () = IFDEF DEBUG THEN print_endline ("Not performing runtime vectorization") ELSE () ENDIF in
      raise (Internal_compiler_error "Index argument")
    else
      (* First get the definition of the variable *)
      (* FIXME: We have assumed that this scalar is invariant in the loop *)
      let symbol = try get symbol_table (get_symbol x) with | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
      let typ = (match symbol with | SimTypedSymbol (x,_,_) -> x | _ -> raise (Internal_compiler_error "Currently addreref type vectorization is not supported!!")) in
      let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
      Vector (typ, s, size, lc)

  | VecRef _ | Constvector _ as s -> s


and build_collapsed_addressed_symbol_vec indices limits symbol_table for_indices lc = function
  | AddressedSymbol (x,_,y,lc) as s ->
    (* First attach yourself into array_inverse *)
    let yexpr = ref (List.rev ((match (List.hd (y)) with BracDim x -> x))) in
    let yexpr = 
      if !my_transpose then
	(* Try to transpose the indices of equal access_sizes to put all the
	   par indices in rows *)
	(* FIXME: This is a special case where only VarRef types are being
	   transposed.  I am not doing expression transposes, because they
	   require one to calculate the exact interation vectors!! Will do
	   that later on!! *)
	let used = List.flatten (List.map (fun (DimSpecExpr x) -> get_used_indices_and_access_sizes indices limits for_indices x) (List.rev !yexpr)) in
	(* After this the yexr_copy list might be completely changed!! *)
	let () = List.iteri (fun i (DimSpecExpr x) -> 
	  let droped_list = List.drop (i+1) !yexpr in
	  transpose_indices indices limits for_indices used i droped_list yexpr x) 
	  (!yexpr) in
	List.rev !yexpr
      else List.rev !yexpr in
    let converted = ref false in
    let yexpr = List.map (fun (DimSpecExpr x) -> 
      try 
	let ret = build_collaped_vec_simexpr_1 true indices limits symbol_table for_indices lc x in 
	converted := true; 
	let () = IFDEF DEBUG THEN print_endline ("Converted is: " ^ (string_of_bool !converted)) ELSE () ENDIF in
	ret 
      with
	| _ -> 
	  let () = IFDEF DEBUG THEN print_endline ("Converted is: " ^ (string_of_bool !converted)) ELSE () ENDIF in
	  if !converted then raise (Error "No possible transpose could help vectorize this loop (try -floop-runtime-vectorize this is currently an unsafe operation)!!") 
	  else x) yexpr in
    if not !converted then raise (Error "Cannot convert to vector type try -floop-runtime-vectorize (this is currently an unsafe operation)");
    let access_sizes = List.map2 (fun (x,y,z) v -> ((get_symbol v),(get_access_size x y z))) limits indices in
    let lone = match (List.nth access_sizes (List.length access_sizes - 1)) with | (x,_) -> x in
    let access_sizes = List.rev (List.tl (List.rev access_sizes)) in
    let access_sizes = access_sizes @ [(lone,1)] in
    (* We have not changed anything here *)
    VecAddress (x, access_sizes, yexpr, lc)

(* This is the lhs side *)
let build_collapsed_vecs indices limits symbol_table for_indices lc = function
  | AllAddressedSymbol x ->
    let () = IFDEF DEBUG THEN List.iter (fun (vstart,vend,vstride) -> 
      print_endline ("LVALUE par bounds: (start:) " 
		     ^(string_of_int vstart) ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride))) limits ELSE () ENDIF in
    let vecad = build_collapsed_addressed_symbol_vec indices limits symbol_table for_indices lc x in
    let (access_sizes,dims) = (match vecad with | VecAddress (_,x,y,_) -> (x,y)) in
    let shuffle_mask = List.mapi (fun i x -> try (i,(calculate_shuffle_mask access_sizes lc x)) with | Ignore _ -> (i,x) | _ -> raise (Internal_compiler_error "")) dims in
    (* Get the constant vectors out *)
    let shuffle_mask = List.filter (fun (_,x) -> filter_shm x) shuffle_mask in
    (* First we need to get the integer variables from these *)
    let indices_converted = List.map (fun (x,_) -> x) shuffle_mask in
    (* let shuffle_mask = List.map (fun (_,Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x)) x) shuffle_mask in *)
    let () = IFDEF DEBUG THEN print_endline "The shuffle_mask is: " ELSE () ENDIF in
    let () = IFDEF DEBUG THEN List.iter (fun x -> print_endline (Dot.dot_simpleexpr x) x) shuffle_mask ELSE () ENDIF in
    let symbol = try get symbol_table (get_symbol (match x with | AddressedSymbol (x,_,_,_) -> x)) with 
      | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let () = IFDEF DEBUG THEN print_endline "Calculating the tot_size" ELSE () ENDIF in
    (* Total size is only going to be used for calculating the inverse
       mask *)
    let (tot_size,to_calc) = (match symbol with ComTypedSymbol (_,x,_) -> 
      (match x with | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with | BracDim x -> 
	(* First we need to filter out the ones, which will be converted *)
	(* Only the indices that we have converted, but when doing the
	   the runtime vectorization we also need to take care of the
	   vector types *)
	let to_calc = List.mapi (fun i (DimSpecExpr x) -> if (List.exists (fun x -> x = i) indices_converted) then x else TStar) x in
	let to_calc = List.filter (fun x -> (match x with TStar -> false | _ -> true)) to_calc in
	(((List.fold_right (fun (Const (_,x,_)) y -> ((int_of_string x)) * y) to_calc 1)-1),to_calc)))
      | _ -> raise (Internal_compiler_error "")) in
    (* Get the interger dimensions from the array declaration only for
       those that got converted*)
    let to_calc = List.map (fun (Const(_,x,_)) -> (int_of_string x)) to_calc in
    let () = IFDEF DEBUG THEN print_endline "The shuffle_mask length is: " ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline (string_of_int (List.length shuffle_mask)) ELSE () ENDIF in
    let shuffle_mask = List.mapi (fun i x -> 
      let to_calc = List.drop (i+1) to_calc in
      let multiplicand = List.fold_right (fun x y -> x * y) to_calc 1 in
      multiply_indices multiplicand x) (List.map (fun (_,x)->x) shuffle_mask) in
    let () = IFDEF DEBUG THEN print_endline "The shuffle_mask after multplication is: " ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline ("Length: " ^ (string_of_int (List.length shuffle_mask))) ELSE () ENDIF in
    let () = IFDEF DEBUG THEN List.iter (fun x -> (Array.iter (fun x -> print_endline (string_of_int x) ) x)) shuffle_mask ELSE () ENDIF in

    (* Add all the constantvectors in the compiler itself the non
       constants need to be added at runtime. The result is either a
       constantvector or a simple expression list *)
    (* Filtering out the constant vectors *)
    let smm = List.filter (fun x -> (match x with Constvector _ -> true | _ -> false)) shuffle_mask in
    let shuffle_mask = 
      if not (List.is_empty smm) then
	let smmv = List.filter (fun x -> (match x with Constvector _ -> false | _ -> true)) shuffle_mask in
	let smma = List.map (fun (Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x)) x) smm in
	let (a,b,c) = (match List.hd (smm) with | Constvector (a,b,_,lv) -> (a,b,lv)) in
	let rsm = Array.init (Array.length (List.hd smma)) (fun i -> 0) in
	let () = List.iter (fun x -> Array.iteri (fun i x -> rsm.(i) <- rsm.(i) + x) x) smma in
	(* The ordering of the shuffle mask should not matter, bcecause it
	   is an addition operation which is assocative and commutative *)
	Constvector (a,b,(Array.map (fun x -> Const(DataTypes.Int32s,(string_of_int x),lc)) rsm),c) :: smmv  
      else shuffle_mask in

    (* How to build the reverse shuffle mask?? *)
    (* Look at the ../programs/test_bitcast.ll file to see how the
       runtime shuffle masks are taken care off!! *)
    let () = IFDEF DEBUG THEN print_endline ("tot_size: " ^ (string_of_int tot_size)) ELSE () ENDIF in
    let (inv_mask,shuffle_mask) =
      if not !my_runtime_vec then
	let smma = List.map (fun (Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x)) x) shuffle_mask in
	let smma = List.hd smma in
	let tot_mask = build_counter_mask 0 tot_size in
	(build_inverse_shuffle_mask tot_mask smma lc, [])
      else
	let smmv = List.filter (fun x -> (match x with Constvector _ -> false | _ -> true)) shuffle_mask in
	if List.is_empty smmv then
	  let smma = List.map (fun (Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x)) x) shuffle_mask in
	  let smma = List.hd smma in
	  let tot_mask = build_counter_mask 0 tot_size in
	  (build_inverse_shuffle_mask tot_mask smma lc,[])
	else
	  ([||], shuffle_mask) in
    AllVecSymbol ({ism = inv_mask;sm = shuffle_mask}, vecad)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got non addressed symbol assignment, currently this is not supported!!"))


let build_collapsed_vec_simexpr indices limits symbol_table for_indices lc = function
  | SimExpr x -> SimExpr (build_collaped_vec_simexpr_1 false indices limits symbol_table for_indices lc x)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "trying to convert a non simple expression to vec type"))

let build_collapsed_data_parallel_vectors indices limits for_indices symbol_table = function
  | Assign (x,y,lc,sp) -> 
    let lvals = List.map (build_collapsed_vecs indices limits symbol_table for_indices lc) x in
    let rvals = build_collapsed_vec_simexpr indices limits symbol_table for_indices lc y in
    Assign (lvals,rvals,lc,sp)
  | Noop -> Noop
  | _ as s -> raise (Internal_compiler_error (("Got erroneously: ") ^ (Dot.dot_stmt s)))

let rec collapse_par indices limits for_indices symbol_table = function
  | Block (x,lc) -> Block ((List.map (collapse_par indices limits for_indices symbol_table) x), lc)
  | Par (x,y,z,lc) ->
    let (vstart,vend,vstride) = get_par_bounds y in
    let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					     ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
    collapse_par (indices @ [x])  (limits@[(vstart,vend,vstride)]) for_indices symbol_table z
  | For _ ->
    (* Just call loop interchange here!! *)
    raise (Internal_compiler_error " Cannot collapse loops, because For detected, you should apply loop interchange with -floop-interchange")
  | _ as s -> build_collapsed_data_parallel_vectors indices limits for_indices symbol_table s

let convert runtim_vec tranpose for_indices symbol_table = function
  | Par (x,y,z,lc) ->
    my_transpose := tranpose;
    my_runtime_vec := runtim_vec;
    let (vstart,vend,vstride) = get_par_bounds y in
    let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					     ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
    collapse_par [x]  [(vstart,vend,vstride)] for_indices symbol_table z

  | _ as s -> raise (Internal_compiler_error (" Cannot parallelize : " ^ (Dot.dot_stmt s)))

let internal_convert symbol_table = function
  | Par (x,y,z,lc) ->
    let (vstart,vend,vstride) = get_par_bounds y in
    let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					     ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
    collapse_par [x]  [(vstart,vend,vstride)] [] symbol_table z

  | _ as s -> raise (Internal_compiler_error (" Cannot parallelize : " ^ (Dot.dot_stmt s)))
