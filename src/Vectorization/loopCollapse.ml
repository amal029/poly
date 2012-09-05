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


let build_collapsed_addressed_symbol_vec indices limits symbol_table ls = function
  | AddressedSymbol (x,_,y,lc) -> 
    (* First get the indices to convert *)
    if (List.length indices) <> (match (List.hd y) with | BracDim x -> x) then raise (Internal_compiler_error "Array Dereferencing not equal to number of loop indices");
    (* Now search each index separately from the indices list and
       check that the length matches the limits for that index
       dereferencing*)
    let indices_to_convert = List.map (fun x -> index_to_convert_to lc x (List.hd y)) indices in
    let sym_indcies = List.map (fun (x,_) -> x) indices_to_convert in
    let symbol = try get symbol_table (get_symbol x) with | Not_found -> raise (Internal_compiler_error "Symbol being dereferenced cannot be found in the table!") in
    let lengths = (match symbol with | ComTypedSymbol (_,AddressedSymbol (_,_,x,_)) ->  (match x with | BracDim x -> 
      (* Now we just want the indices in order specified by sym_indices *)
      List.map (fun io -> match (List.findi (fun i x -> (i = io)) x) with | (_,x) -> x) sym_indices
      | _ -> raise (Internal_compiler_error "Symbol not of type aggregate"))) in
    (* Now we can make sure that all the limits are within range of lengths *)
    let lengths = List.map (get_com_dims lc) lengths in
    (* Now we have int types in lengths *)
    let lengths = List.map (fun Const (_,x,_) -> (int_of_string x)) lengths in
    (* Now we have const types in lengths, else we get an error *)
    let () = IFDEF DEBUG THEN print_endline ((string_of_int (List.length lengths))) ELSE () ENDIF in
    (* Now get the access sizes for all the limits *)
    let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
    (* Now make sure that all the indices are within limits *)
    let () = List.iter2 (fun x y -> if not ( x <= y) then raise (Internal_compiler_error "Access sizes not within bounds")) access_sizes lengths in
    (* Now finally we can make the colon expression for this dereference point *)
    let ces = List.map (fun (vstart, vend, vstride) -> DimSpecExpr (ColonExpr (Const (DataTypes.Int32s, string_of_int vstart, lc),Const(DataTypes.Int32s, string_of_int vend, lc),
								  Const(DataTypes.Int32s, string_of_int vstride, lc),lc))) in
    (* Now convert this to a DimSpecExpr and BracDim please *)
    let ces = BracDim ces in
    VecAddress (x, ces, lc)

let build_collapsed_vecs indices limits symbol_table lc = function
  | AllAddressedSymbol x -> 
    let () = IFDEF DEBUG THEN List.iter (fun (vstart,vend,vstride) -> 
      print_endline ("par bounds: (start:) " ^ (string_of_int vstart) ^ "(end:) " ^ (string_of_int vend) ^ "(stride:) " ^ (string_of_int vstride))) limits in
    AllVecSymbol (build_collapsed_addressed_symbol_vec indices limits symbol_table lc x)
  | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got non addressed symbol assignment, currently this is not supported!!"))

let cartresian_product indices = function
  | (VarRef (x,_), VarRef (y,_)) -> (List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices) && (List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices)
  | _ -> false

let strech_vector indices = function
  | ((VarRef (x,_)), AddrRef_) -> ((List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices),0)
  | (AddrRef_, (VarRef (x,_))) -> ((List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices),1)
  | _ as s -> (false,s)

let rec build_collaped_vec_simexpr_1 indices limits symbol_table lc = function
  | Plus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.add Float.add) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
      (* Strech the vector *)
	if s = 0 then
	  (* Strech the first vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Plus(Constvector (t,aa),r)
	else 
	  (*Strech the second vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Plus(l,Constvector (t,aa))
      else Plus (l,r,lc)

  | Minus (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.sub Float.sub) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
      (* Strech the vector *)
      if s = 0 then
	(* Strech the first vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Minus(Constvector (t,aa),r)
      else 
	(*Strech the second vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Minus(l,Constvector (t,aa))
    else Minus (l,r,lc)
  | Times (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.mul Float.mul) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
      let (doit,s) = strech_vector l v in
    else 
      let (doit,s) = strech_vector l v in
      if doit then
      (* Strech the vector *)
      if s = 0 then
	(* Strech the first vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Times(Constvector (t,aa),r)
      else 
	(*Strech the second vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Times(l,Constvector (t,aa))
    else Times (l,r,lc)

  | Div (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.div Float.div) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
      (* Strech the vector *)
      if s = 0 then
	(* Strech the first vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Div(Constvector (t,aa),r)
      else 
	(*Strech the second vector *)
	let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	(* We need to strech it by the division of the two sizes *)
	let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	Div(l,Constvector (t,aa))
    else Div (l,r,lc)

  | Pow (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.pow Float.pow) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
      if doit then
	(* Strech the vector *)
	if s = 0 then
	  (* Strech the first vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Pow(Constvector (t,aa),r)
	else 
	  (*Strech the second vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Pow(l,Constvector (t,aa))
      else Pow (l,r,lc)

  | Mod (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y Int.rem Float.modulo) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
	(* Strech the vector *)
	if s = 0 then
	  (* Strech the first vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Mod(Constvector (t,aa),r)
	else 
	  (*Strech the second vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Mod(l,Constvector (t,aa))
      else Mod (l,r,lc)

  | Rshift (x,y,lc) -> 

    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y (lsr) (Constantfolding.Constantpropogation.lsd)) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
	(* Strech the vector *)
	if s = 0 then
	  (* Strech the first vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Rshift(Constvector (t,aa),r)
	else 
	  (*Strech the second vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Rshift(l,Constvector (t,aa))
    else Rshift (l,r,lc)

  | Lshift (x,y,lc) -> 

    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc x in
    let r = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    if cartresian_product (x,y) then
      (* Then actually perform some computation *)
      let (lv,rv) = (match (l,r) with
	| Constvector (_,x), Constvector(_,y) -> (x,y)
	| _ -> raise (Internal_compiler_error "Induction vars not converted to constvector types!!"))
      let cl = List.cartresian_product Array.to_list (lv) Array.to_list (rv) in
      let vevl = List.map (fun (Const(x,y,_),Const(z,g,_)) -> (Consts.VConst(x,y), Consts.VConst(z,g))) cl in
      let evall = List.map (fun (x,y) -> Constantfolding.Constantpropogation.process x y (lsl) (Constantfolding.Constantpropogation.lsd)) vevl in
      let retl = List.map (fun x -> VConst (x,y) -> Const(x,y,lc) | _ -> raise (Internal_compiler_error "Cannot happen")) evall in
      Constvector retl
    else 
      let (doit,s) = strech_vector l v in
      if doit then
	(* Strech the vector *)
	if s = 0 then
	  (* Strech the first vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match l with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Lshift(Constvector (t,aa),r)
	else 
	  (*Strech the second vector *)
	  let access_sizes = List.map (fun (x,y,z) -> get_access_size x y z) limits in
	  (* We need to strech it by the division of the two sizes *)
	  let (to_strech_size,a,t) = access_sizes / (match r with | Constvector (t,x) -> ((Array.length x),x,t) | _ -> raise (Internal_compiler_error "")) in
	  let aa = Array.of_list (List.flatten (Array.to_list (Array.init to_strech_size (fun i -> (Array.to_list a))))) in
	  Lshift(l,Constvector (t,aa))
    else Lshift (l,r,lc)

  | Brackets (x,lc) -> Brackets (build_collaped_vec_simexpr_1 indices limits symbol_table lc x, lc)

  (* FIXME: (Improve) Cannot cast need to make a vector type cast!! *)
  | Cast (x,y,lc) -> 
    let l = build_collaped_vec_simexpr_1 indices limits symbol_table lc y in
    (* If this is a const matrix then it is easy to cast else for now give an error type!! *)
    (match l with 
      | Constvector (_,x1,lc) -> Constvector(x,x1,lc)
      | _ -> raise (Internal_compiler_error "Cannot cast non const vector types yet!! "))

  | Opposite (x,lc) -> Opposite (build_collaped_vec_simexpr_1 indices limits symbol_table lc x, lc)
  | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))
  | Const (x,y,lc) ->
      (* Special case expand the vector to the required size and give a const vector back!! *)
      (* let size = (vend - vstart + 1)/vstride in  *)
    let size = List.fold_right(fun (vstart,vend,vstride) x -> (get_access_size vstart vend vstride) * x) limits 1 in
    let () = IFDEF DEBUG THEN print_endline ("Array size: " ^ (string_of_int size)) ELSE () ENDIF in
    Constvector (x,(Array.init size (fun i -> Const (x,y,lc))),lc)
  | AddrRef (x,lc) -> VecRef (build_collapsed_addressed_symbol_vec indices limits symbol_table lc x, lc)
  | VarRef (x,lc) as s ->
    (* Induction variables can only be of type Int32s *)
    if List.exists (fun i -> (get_symbol x) = (get_symbol i)) indices then
      let (index,_) = List.findi (fun i -> (get_symbol i) = (get_symbol x)) indices in
      let (vstart,vend,vstride) = List.nth limits index in
      (* Now get the limit [index], which has this symbol *)
      let size = get_access_size vstart vend vstride in
      let counter = ref vstart in
      let ar = Array.init size (fun i -> let ret = !counter in counter := !counter + vstride; ret) in
      let ar = Array.map (fun x -> Const(DataTypes.Int32s, (string_of_int x), lc)) ar in
      Constvector (DataTypes.Int32s, ar, lc)
    else raise (Error ((Reporting.get_line_and_column lc) ^ "Not an induction loop: " ^ Dot.dot_simpleexpr s))
  | VecRef _ | Constvector _ as s -> s

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
  | Block (x,_) -> collapse_par indices symbol_table x
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
