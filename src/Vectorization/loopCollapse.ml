(**

   Wed Sep  5 12:08:23 IST 2012
   
   Purpose: 
   
   Carries out outer loop collapsing
   
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

(* let process_vectors f1 f2 = function *)
(*   | (Constvector (_,d,x,_) , Constvector (_,d,y,lc)) -> *)
(*     Constvector (None,d,(Array.map2 (fun (Const(d,x,_),Const(t,y,_)) -> Constantfolding.Constantpropogation.process (VConst.VConst(d,x)) (VConst.VConst(t,y)) f1 f2) x y), *)
(* 		 lc) *)
(*   | _ -> raise (Internal_compiler_error "") *)

let rec build_collaped_vec_simexpr_1 indices limits symbol_table lc = function
  | Plus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Plus (l,r,lc)

  | Minus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Minus (l,r,lc)

  | Times (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Times (l,r,lc)

  | Div (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Div (l,r,lc)

  | Pow (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Pow (l,r,lc)

  | Mod (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Mod (l,r,lc)

  | Rshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Rshift (l,r,lc)

  | Lshift (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    Lshift (l,r,lc)

  | Brackets (x,lc) -> Brackets (build_collaped_vec_simexpr_1 indices limits symbol_table lc x, lc)

  (* FIXME: (Improve) Cannot cast need to make a vector type cast!! *)
  | Cast (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    (* If this is a const matrix then it is easy to cast else for now give an error type!! *)
    (match l with 
      | Constvector (r,_,x1,lc) -> Constvector(r,x,x1,lc)
      | _ -> raise (Internal_compiler_error "Cannot cast non const vector types yet!! "))

  | Opposite (x,lc) -> Opposite (build_collaped_vec_simexpr_1 indices limits symbol_table lc x, lc)

  | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))

  | Const (x,y,lc) ->
      (* Special case expand the vector to the required size and give a const vector back!! *)
      (* let size = (vend - vstart + 1)/vstride in  *)
    let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
    let () = IFDEF DEBUG THEN print_endline ("Array size: " ^ (string_of_int size)) ELSE () ENDIF in
    Constvector (None,x,(Array.init size (fun i -> Const (x,y,lc))),lc)

  | AddrRef (x,lc) -> VecRef (build_collapsed_addressed_symbol_vec indices limits symbol_table lc x, lc)

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
    else 
      (* First get the definition of the variable *)
      (* We have assumed that this scalar is invariant in the loop *)
      let symbol = try get symbol_table (get_symbol x) with | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
      let typ = (match symbol with | SimTypedSymbol (x,_,_) -> x) in
      let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
      Vector (x, Array.init size (fun i -> s),lc)

  | VecRef _ | Constvector _ as s -> s

and build_collapsed_addressed_symbol_vec indices limits symbol_table lc = function
  | AddressedSymbol (x,_,y,lc) as s -> 
    (* First attach yourself into array_inverse *)
    let yexpr = (match (List.hd (y)) with BracDim x -> x) in
    let yexpr = build_ad_vecs yexpr in
    (* Then get the size of all the parents *)
    let access_sizes = List.map2 (fun (x,y,z) v -> ((match v with VarRef (x,_)->(get_symbol x)),(get_access_size x y z))) limits indices in
    let access_sizes = List.rev (List.tl (List.rev access_sizes)) in
    VecAddress (x, access_sizes, yexpr, lc)

and build_ad_vecs indices limits symbol_table lc = function
  | h::t -> 
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc h  in
    r :: build_collaped_vec_simexpr_1 indices limits symbol_table lc t 
  | [] -> []

let build_collapsed_vecs indices limits symbol_table lc = function
  | AllAddressedSymbol x -> 
    let () = IFDEF DEBUG THEN List.iter (fun (vstart,vend,vstride) -> 
      print_endline ("par bounds: (start:) " ^ (string_of_int vstart) ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride))) limits in
    let vecad = build_collapsed_addressed_symbol_vec indices limits symbol_table lc x in
    let (access_sizes,dims) = (match vecad with | VecAddress (_,x,y,_) -> (x,y)) in
    let shuffle_mask = List.map (calculate_shuffle_mask access_sizes) dims in
    let symbol = try get symbol_table (get_symbol (match x with | AddressedSymbol (x,_,_,_) -> x)) with 
      | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let tot_size = (match symbol with ComTypedSymbol (_,x) -> 
      (match x with | AddressedSymbol (_,_,x,_) -> (match (List.hd x) with | BracDim x -> (List.fold_right (fun (Const (_,x,_)) y -> (int_of_string x) * y) x 1)))
      | _ -> raise (Internal_compiler_error "")) in
    let tot_mask = build_counter_mask 0 tot_size in
    let inv_mask = build_inverse_shuffle_mask tot_mask shuffle_mask lc in
    AllVecSymbol (inv_mask, vecad)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got non addressed symbol assignment, currently this is not supported!!"))


let build_collapsed_vec_simexpr indices limits symbol_table lc = function
  | SimExpr x -> SimExpr (build_collaped_vec_simexpr_1 indices limits symbol_table lc x)
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
