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

module Array = Batteries.Array
module List = Batteries.List
module Int = Batteries.Int
module Float = Batteries.Float

let strech_looking_at_child size list = 
  Array.of_list (List.map (fun x -> Array.to_list (Array.init size (fun i -> x))) list)

let strech_looking_at_parent size list = 
  Array.of_list (List.flatten (Array.to_list (Array.init size (fun i -> list))))

let rec build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc = function
  | Plus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Plus (l,r,lc)

  | Minus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Minus (l,r,lc)

  | Times (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Times (l,r,lc)

  | Div (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Div (l,r,lc)

  | Pow (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Pow (l,r,lc)

  | Mod (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Mod (l,r,lc)

  | Rshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Rshift (l,r,lc)

  | Lshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    Lshift (l,r,lc)

  | Brackets (x,lc) -> Brackets (build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x, lc)

  (* FIXME: (Improve) Cannot cast need to make a vector type cast!! *)
  | Cast (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc y in
    (* If this is a const matrix then it is easy to cast else for now give an error type!! *)
    (match l with 
      | Constvector (r,_,x1,lc) -> Constvector(r,x,x1,lc)
      | _ -> raise (Internal_compiler_error "Cannot cast non const vector types yet!! "))

  | Opposite (x,lc) -> Opposite (build_collaped_vec_simexpr_1 index_arg indices limits symbol_table lc x, lc)

  | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))

  | Const (x,y,lc) ->
      (* Special case expand the vector to the required size and give a const vector back!! *)
      (* let size = (vend - vstart + 1)/vstride in  *)
    let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
    let () = IFDEF DEBUG THEN print_endline ("Array size: " ^ (string_of_int size)) ELSE () ENDIF in
    Constvector (None,x,(Array.init size (fun i -> Const (x,y,lc))),lc)

  | AddrRef (x,lc) -> 
    let vecad = build_collapsed_addressed_symbol_vec indices limits symbol_table lc x in
    let (access_sizes,dims) = (match vecad with | VecAddress (_,x,y,_) -> (x,y)) in
    let shuffle_mask = List.mapi (fun i x -> try (i,(calculate_shuffle_mask access_sizes lc x)) with | Ignore _ -> (i,x) | _ -> raise (Internal_compiler_error "")) dims in
    let shuffle_mask = List.filter (fun (_,x) -> (match x with Constvector _ -> true | _ -> false)) shuffle_mask in
    (* First we need to get the integer variables from these *)
    let indices_converted = List.map (fun (x,_) -> x) shuffle_mask in
    let shuffle_mask = List.map (fun (_,Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x))) shuffle_mask in
    let symbol = try get symbol_table (get_symbol (match x with | AddressedSymbol (x,_,_,_) -> x)) with 
      | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let (tot_size,to_calc) = (match symbol with ComTypedSymbol (_,x) -> 
      (match x with | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with | BracDim x -> 
	(* First we need to filter out the ones, which will be converted *)
	let to_calc = List.mapi (fun i (DimSpecExpr x) -> if (List.exists (= i) indices_converted) then x else TStar) x in
	let to_calc = List.filter (fun x -> (match x with TStar -> false | _ -> true)) to_calc in
	((List.fold_right (fun (Const (_,x,_)) y -> (int_of_string x) * y) to_calc 1),to_calc)))
      | _ -> raise (Internal_compiler_error "")) in
    (* First get the parent indices *)
    let parent_indices = List.take ((List.length indices) -1) indices in
    let go_through = (match x with AddressedSymbol (_,_,x,_) -> (match x with BracDim x -> x)) in
    let go_through = List.mapi (fun i (DimSpecExpr x) -> if (List.exists (= i) indices_converted) then x else TStar) go_through in
    let go_through = List.filter (fun x -> (match x with TStar -> false | _ -> true)) go_through in
    let () = assert (List.length shuffle_mask = List.length go_through) in
    let () = assert (List.length shuffle_mask = List.length to_calc) in
    let shuffle_mask = if (List.length shuffle_mask = 1) then shuffle_mask 
      else get_updated_shuffle_mask 0 shuffle_mask to_calc parent_indices go_through in
    (* Now find out which of these dimenions need to be updated and update them correctly *)
    (* Now first get the indices used in any given dimension *)
    let len = Array.length (List.hd shuffle_mask) in
    let rsm = Array.init len (fun i -> 0) in
    let () = List.iter (fun x -> Array.iteri (fun i x -> rsm.(i) <- rsm.(i) + x) x) shuffle_mask in
    VecRef (rsm,vecad,lc)

  | VarRef (x,lc) as s ->
    (* Induction variables can only be of type Int32s (for now)!! *)
    if List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices then
      let (index,_) = List.findi (fun i -> (get_symbol i) = (get_symbol x)) indices in
      let (vstart,vend,vstride) = List.nth limits index in
      (* This is your own repitition vector *)
      let my_size = get_access_size vstart vend vstride in
      (* Get the repitition vectors above you *)
      let (plist, clist) = List.split_at index limits in
      let plist = List.take (index-1) plist in
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
      let ar = Array.init size (fun i -> let ret = !counter in counter := !counter + vstride; ret) in
      (* Now we need to strech this constant array by first looking at child and then looking at parent *)
      let ar = if clist <> [] then strech_looking_at_child child_size (Array.to_list ar) else ar in
      let ar = if plist <> [] then strech_looking_at_parent parent_size (Array.to_list ar) else ar in
      let ar = Array.map (fun x -> Const(DataTypes.Int32s, (string_of_int x), lc)) ar in
      Constvector ((get_symbol x), DataTypes.Int32s, ar, lc)
    (* Now we start taking care of scalars, which are tops as well *)
    else if index then raise (Internal_compiler_error " Index argument ")
    else 
      (* First get the definition of the variable *)
      (* We have assumed that this scalar is invariant in the loop *)
      let symbol = try get symbol_table (get_symbol x) with | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
      let typ = (match symbol with | SimTypedSymbol (x,_,_) -> x) in
      let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
      Vector (x, s, size, lc)

  | VecRef _ | Constvector _ as s -> s

and build_collapsed_addressed_symbol_vec indices limits symbol_table lc = function
  | AddressedSymbol (x,_,y,lc) as s -> 
    (* First attach yourself into array_inverse *)
    let yexpr = (match (List.hd (y)) with BracDim x -> x) in
    let converted = ref false in
    let yexpr = List.map (fun (DimSpecExpr x) -> try let ret = build_collaped_vec_simexpr_1 true indices limits symbol_table lc x in converted := true; ret with 
      | _ -> if converted then raise (Error "Currently we do not support interchanged, but not transposed access to multi-dimensional Arrays") else x) yexpr in
    let access_sizes = List.map2 (fun (x,y,z) v -> ((match v with VarRef (x,_)->(get_symbol x)),(get_access_size x y z))) limits indices in
    let lone = match (List.nth access_sizes (List.length access_sizes - 1)) with | (x,_) -> x in
    let access_sizes = List.rev (List.tl (List.rev access_sizes)) in
    let access_sizes = access_sizes @ [(lone,1)] in
    VecAddress (x, access_sizes, yexpr, lc)

let build_collapsed_vecs indices limits symbol_table lc = function
  | AllAddressedSymbol x ->
    let () = IFDEF DEBUG THEN List.iter (fun (vstart,vend,vstride) -> 
      print_endline ("par bounds: (start:) " ^ (string_of_int vstart) ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride))) limits in
    let vecad = build_collapsed_addressed_symbol_vec indices limits symbol_table lc x in
    let (access_sizes,dims) = (match vecad with | VecAddress (_,x,y,_) -> (x,y)) in
    let shuffle_mask = List.mapi (fun i x -> try (i,(calculate_shuffle_mask access_sizes lc x)) with | Ignore _ -> (i,x) | _ -> raise (Internal_compiler_error "")) dims in
    let shuffle_mask = List.filter (fun (_,x) -> (match x with Constvector _ -> true | _ -> false)) shuffle_mask in
    (* First we need to get the integer variables from these *)
    let indices_converted = List.map (fun (x,_) -> x) shuffle_mask in
    let shuffle_mask = List.map (fun (_,Constvector (_,_,x,_)) -> Array.map (fun (Const(_,x,_)) -> (int_of_string x))) shuffle_mask in
    let symbol = try get symbol_table (get_symbol (match x with | AddressedSymbol (x,_,_,_) -> x)) with 
      | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let (tot_size,to_calc) = (match symbol with ComTypedSymbol (_,x) -> 
      (match x with | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with | BracDim x -> 
	(* First we need to filter out the ones, which will be converted *)
	let to_calc = List.mapi (fun i (DimSpecExpr x) -> if (List.exists (= i) indices_converted) then x else TStar) x in
	let to_calc = List.filter (fun x -> (match x with TStar -> false | _ -> true)) to_calc in
	((List.fold_right (fun (Const (_,x,_)) y -> (int_of_string x) * y) to_calc 1),to_calc)))
      | _ -> raise (Internal_compiler_error "")) in
    (* First get the parent indices *)
    let parent_indices = List.take ((List.length indices) -1) indices in
    let go_through = (match x with AddressedSymbol (_,_,x,_) -> (match x with BracDim x -> x)) in
    let go_through = List.mapi (fun i (DimSpecExpr x) -> if (List.exists (= i) indices_converted) then x else TStar) go_through in
    let go_through = List.filter (fun x -> (match x with TStar -> false | _ -> true)) go_through in
    let () = assert (List.length shuffle_mask = List.length go_through) in
    let () = assert (List.length shuffle_mask = List.length to_calc) in
    let shuffle_mask = if (List.length shuffle_mask = 1) then shuffle_mask 
      else get_updated_shuffle_mask 0 shuffle_mask to_calc parent_indices go_through
    (* Now find out which of these dimenions need to be updated and update them correctly *)
    (* Now first get the indices used in any given dimension *)
    let len = Array.length (List.hd shuffle_mask) in
    let rsm = Array.init len (fun i -> 0) in
    let () = List.iter (fun x -> Array.iteri (fun i x -> rsm.(i) <- rsm.(i) + x) x) shuffle_mask in
    let tot_mask = build_counter_mask 0 tot_size in
    let inv_mask = build_inverse_shuffle_mask tot_mask (Array.to_list rsm) lc in
    AllVecSymbol (inv_mask, vecad)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got non addressed symbol assignment, currently this is not supported!!"))


let build_collapsed_vec_simexpr indices limits symbol_table lc = function
  | SimExpr x -> SimExpr (build_collaped_vec_simexpr_1 false indices limits symbol_table lc x)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "trying to convert a non simple expression to vec type"))

let build_collapsed_data_parallel_vectors indices limits symbol_table = function
  | Assign (x,y,lc) -> 
    let lvals = List.map (build_collapsed_vecs indices limits symbol_table lc) x in
    let rvals = build_collapsed_vec_simexpr indices limits symbol_table lc y in
    Assign (lvals,rvals,lc)
  | Noop -> Noop
  | _ as s -> raise (Internal_compiler_error (("Got erroneously: ") ^ (Dot.dot_stmt s)))

let rec collapse_par indices limits symbol_table = function
  | Block (x,lc) -> Block ((List.map (collapse_par indices limits symbol_table) x), lc)
  | Par (x,y,z,lc) ->
    let (vstart,vend,vstride) = get_par_bounds y in
    let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					     ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
    collapse_par (indices @ [x])  (limits@[(vstart,vend,vstride)]) symbol_table z
  | For _ -> 
    (* Just call loop interchange here!! *)
    raise (Internal_compiler_error " Cannot collapse loops, because For detected, you should apply loop interchange")
  | _ -> build_collapsed_data_parallel_vectors x symbol_table vstart vend vstride z

let convert gsymbol_table = function
  | Par (x,y,z,lc) ->
    let (vstart,vend,vstride) = get_par_bounds y in
    let () = IFDEF DEBUG THEN print_endline ("par bounds: (start:) " ^ (string_of_int vstart)
					     ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride)) ELSE () ENDIF in
    collapse_par [x]  [(vstart,vend,vstride)] symbol_table z

  | _ as s -> raise (Internal_compiler_error (" Cannot parallelize : " ^ (Dot.dot_stmt s)))
