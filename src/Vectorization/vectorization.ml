module SafeToConvert =

struct
  open Language
  open Language
  module List = Batteries.List

  exception Internal_compiler_error of string
      
  let is_fcall = function
    | FCall _ -> true
    | _ -> false

  let is_typed_symbol x = List.fold_right (fun x y -> (match x with | AllAddressedSymbol _ -> true | _ -> false) && y) x true

  let rec is_par_safe_to_convert = function
    | Assign (y,x,_) -> not (is_fcall x) && (not (is_typed_symbol y))
    | Block (x,_) -> List.fold_right (fun x y -> is_par_safe_to_convert x && y) x true
    | Split (x,_) -> is_par_safe_to_convert x
    | Par _ | For _ | CaseDef _ | Escape _ | VarDecl _ -> false
    | _ -> true

  let process_par = function
    | Par (_,_,z,lc) -> is_par_safe_to_convert z
    | _ as s -> raise (Internal_compiler_error ("Got a non par stmt for checking: " ^ (Dot.dot_stmt s)))

end 


module Convert =
struct
  open Language
  open Language
  open CFG
  module Array = Batteries.Array
  module List = Batteries.List

  exception Internal_compiler_error of string
  exception Error of string

  let get_symbol_lc = function
    | Symbol(_,lc) -> lc
  let get_addressed_symbol_lc = function
    | AddressedSymbol(_,_,_,lc) -> lc
      
  let get_typed_symbol_lc = function
    | SimTypedSymbol (_,_,lc) -> lc
    | ComTypedSymbol (_,_,lc) -> lc
    

  let get_symbol = function
    | Symbol (x,_) -> x
  let get_addressed_symbol = function
    | AddressedSymbol (x,_,_,_) -> get_symbol x
  let get_typed_symbol = function
    | SimTypedSymbol (_,x,_) -> get_symbol x
    | ComTypedSymbol (_,x,_) -> get_addressed_symbol x

  let exists declarations s = 
    List.exists (fun x -> (get_typed_symbol x) = s ) declarations

  let get declarations s = 
    List.find (fun x -> (get_typed_symbol x) = s) declarations

      
  (* I am already sure that the same symbols cannot be repeated and also
     that the use of symbols are within scope and hence, I just use this
     to build the symbol table all at once!! *)

  let rec build_symbol_table symbol_table = function
    | VarDecl (x,lc) -> symbol_table := x :: !symbol_table
    | Assign (x,_,lc) -> List.iter (fun x -> (match x with | AllTypedSymbol x -> symbol_table := x :: !symbol_table | _ -> ())) x
    | Block (x,_) -> List.iter (build_symbol_table symbol_table) x
    | CaseDef (x,lc) -> build_case_symbol_table symbol_table x
    | For (_,_,x,_) | Par (_,_,x,_) | Split (x,_) -> build_symbol_table symbol_table x
    | Escape _ -> ()
    | Noop -> ()
  and build_case_symbol_table symbol_table = function
    | Case (x,y,_) -> List.iter (build_clause_list symbol_table) x; build_o_list symbol_table y
  and build_clause_list symbol_table = function
    | Clause (_,x,_) -> build_symbol_table symbol_table x
  and build_o_list symbol_table = function
    | Otherwise (x,_) -> build_symbol_table symbol_table x

  let rec get_const lc = function
    | Const _ as s -> s
    | Brackets (x,lc) -> get_const lc x
    | Opposite (x,lc) -> 
      let r = get_const lc x in
      (match r with
	| Const (x,v,lc) -> 
	  let vr = int_of_string v in Const (x, (string_of_int (-vr)),lc)
	| _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " not of type const: " ^ (Dot.dot_simpleexpr s))))
    | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " not of type const: " ^ (Dot.dot_simpleexpr s)))

  let build_shuffle_mask s e st lc = 
    let shuffle_mask = 
      (try Array.init ((e-s+1)/st) (fun i -> s)
       with
	 | _ as s -> print_endline (Reporting.get_line_and_column lc); raise s) in
    (* Now make the elements of the array correctly!! *)
    let counter = ref s in
    Array.map (fun x -> let ret = !counter in counter := !counter + st; ret) shuffle_mask

  let build_counter_mask s e =  Array.init (e-s+1) (fun i -> i)

  (* This is for the store side *)
  let build_inverse_shuffle_mask s e st lc = 
    let tot = ref (build_counter_mask s e) in
    let sm = build_shuffle_mask s e st lc in
    let () = List.iter (fun x -> tot := (List.filter (fun x ->  x <> i) !tot)) sm in 
    tot
      
  let build_permute_mask tot first second lc = 
    let c1 = 0 in
    let c2 = List.length first in
    Array.map (fun x -> 
      (try
	 Array.findi ((=)x) first + c1
       with
	 | Not_found -> 
	   (try 
	      Array.findi ((=)x) second + c2
	    with
	      | Not_found -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " all hell broke loose when finding the permutation mask"))))) tot

  let get_par_bounds = function
    | ColonExpr (start,e,stride,lc) -> 
      let start = get_const lc start in
      let e = get_const lc e in
      let stride = get_const lc stride in
      (match (start,e,stride) with
	| (Const (_,x,_), Const (_,y,_), Const (_,z,_)) -> (int_of_string x, int_of_string y, int_of_string z)
	| _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " colon expressions not constants")))
    | _ as s -> raise (Internal_compiler_error (" Par loop bounds can only be of type colon expr, but I got: " ^ (Dot.dot_simpleexpr s)))


  let get_com_dims lc = function
    | DimSpecExpr x -> 
      match x with 
	| Const _ as s -> s 
	| _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " array sizes not const"))

  let rec get_index_to_conv lc index = function
    | VarRef (x,_) as s -> index = x
    | Brackets (x,_) -> get_index_to_conv lc index x
    | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " cannot find the index in the Par index list"))

  let get_index_to_convert lc index counter = function
    | DimSpecExpr x -> get_index_to_conv lc index x

  let index_to_convert_to lc index = function
    | BracDim l -> List.findi (get_index_to_convert lc index) l
      
  let check_index lc index = function
    | BracDim l -> List.filter (get_index_to_convert lc index) l

  let build_addressed_symbol_vec index symbol_table vstart vend vstride lc = function
    | AddressedSymbol (x,_,y,lc) -> 
      (* convert the last index into colon expression for later extraction in llvm!! *)
      if (List.length (check_index lc index (List.hd y)) <> 1) then 
	raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " cannot handle more/less than one indexing with same par index!!")) 
      let (index_to_convert,_) = (index_to_convert_to lc index) (List.hd y) in
      let () = IFDEF DEBUG THEN print_endline "Trying to convert index: " ^ (string_of_int index_to_convert_to) ELSE () ENDIF in
      (* First get the typed symbol from the symbol_table *)
      let symbol =  try get symbol_table (get_symbol x) with | Not_found -> 
	raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " var not found in symbol table")) in 
      let access_size = (vend - vstart + 1)/vstride in
      (* Get the size of the complex symbol *)
      let com_list = 
	(match symbol with 
	  | ComTypedSymbol (_,x,_) -> 
	    (match x with
	      | AddressedSymbol (_,_,x,lc) ->
		List.flatten (List.map (fun x -> 
		  (match x with | BracDim x -> List.map (get_com_dims lc) x)) x))) in
      (* Now make sure that all dimensions match *)
      if List.length y = List.length com_list then
	if access_size <= (match (List.nth com_list index_to_convert)  with | Const (_,x,_) -> (int_of_string x) 
	  | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " array indices not const"))) then
	  let ce = ColonExpr (Const (DataTypes.Int32s, string_of_int vstart, lc),Const(DataTypes.Int32s, string_of_int vend, lc),
			      Const(DataTypes.Int32s, string_of_int vstride, lc),lc) in
	  let dl = match (List.hd y) with | BracDim x -> List.map (fun (DimSpecExpr x) -> if x = index then DimSpecExpr ce else DimSpecExpr x) x in
	  VecAddress (x, BracDim dl,lc)
	else raise (Error ((Reporting.get_line_and_column lc) ^ " dereferencing more dimensions than the allowed size for the array!! "))
      else raise (Error ((Reporting.get_line_and_column lc) ^ " iteration vector of the par loop does not fit into the size of the declared array for this dimension"))

  let build_vecs index symbol_table vstart vend vstride lc = function
    | AllAddressedSymbol x -> AllVecSymbol (build_addressed_symbol_vec index symbol_table vstart vend vstride lc x)
    | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " got non addressed symbol assignment, currently this is not supported!!"))

  let rec build_vec_simexpr_1 index symbol_table vstart vend vstride lc = function
    | Plus (x,y,lc) -> Plus (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Minus (x,y,lc) -> Minus (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Times (x,y,lc) -> Times (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Div (x,y,lc) -> Div (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Pow (x,y,lc) -> Pow (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Mod (x,y,lc) -> Mod (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Rshift (x,y,lc) -> Rshift (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Lshift (x,y,lc) -> Lshift (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, 
    build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Brackets (x,lc) -> Brackets (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, lc)
    | Cast (x,y,lc) -> Cast (x,build_vec_simexpr_1 index symbol_table vstart vend vstride lc y, lc)
    | Opposite (x,lc) -> Opposite (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x, lc)
    | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))
    | VarRef _ | VecRef _ | Constvector _ as s -> s
    | Const (x,y,lc) -> 
      (* Special case expand the vector to the required size and give a const vector back!! *)
      let size = (vend - vstart + 1)/vend in Constvector (x,(Array.init size (fun i -> Const (x,y,lc))),lc)
    | AddrRef (x,lc) -> VecRef (build_addressed_symbol_vec index symbol_table vstart vend vstride lc x, lc)

  let build_vec_simexpr index symbol_table vstart vend vstride lc = function
    | SimExpr x -> SimExpr (build_vec_simexpr_1 index symbol_table vstart vend vstride lc x)
    | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ "trying to convert a non simple expression to vec type"))

  let rec build_data_parallel_vectors index symbol_table vstart vend vstride = function
    | Block (x,lc) -> Block (List.map (build_data_parallel_vectors index symbol_table vstride vend vstride) x, lc)
    | Assign (x,y,lc) ->
      (* Now start making the changes to the assign statement *)
      let lvals = List.map (build_vecs index symbol_table vstart vend vstride lc) x in
      let rvals = build_vec_simexpr index symbol_table vstart vend vstride lc y in
      Assign (lvals,rvals,lc)
    | Noop -> Noop
    | _ as s -> raise (Internal_compiler_error (("Got erroneously: ") ^ (Dot.dot_stmt s)))

  let convert symbol_table = function
    | Par (x,y,z,lc) -> 
      (*First get the size of the vector *)
      (* FIXME: This needs to be extended to affine strides and bounds!! *)
      let (vstart,vend,vstride) = get_par_bounds y in
      (* Now build the internal data-parallel vectors *)
      build_data_parallel_vectors x symbol_table vstart vend vstride z
    | _ as s -> raise (Internal_compiler_error (" Cannot parallelize : " ^ (Dot.dot_stmt s)))
end 
