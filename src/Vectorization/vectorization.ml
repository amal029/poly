module SafeToConvert =

struct
  open Language
  open Language
  module List = Batteries.List

  exception Internal_compiler_error of string
      
  let is_fcall = function
    | FCall _ -> true
    | _ -> false

  let is_typed_symbol x = 
    let ret = List.fold_right (fun x y -> (match x with | AllTypedSymbol _ as s -> Dot.dot_allsym s; true
      | _ -> false) && y) x true in
    let () = IFDEF DEBUG THEN print_endline ("Assign lvalue not AddressedSymbol or Vector Type") ELSE () ENDIF in ret

  let rec is_par_safe_to_convert = function
    | Assign (y,x,lc,sp) -> 
      let ret = (not (is_fcall x)) && (not (is_typed_symbol y)) in
      let () = IFDEF DEBUG THEN print_endline ("Assign at stmt "^ (Reporting.get_line_and_column lc) ^ "can be vectorized: " ^ (string_of_bool ret)) ELSE () ENDIF in
      ret
    | Block (x,lc) -> 
      let ret = List.fold_right (fun x y -> is_par_safe_to_convert x && y) x true in
      let () = IFDEF DEBUG THEN print_endline ("Block can be vectorized: " ^ (string_of_bool ret)) ELSE () ENDIF in
      ret
    | Split _ | CaseDef _ | Escape _ | VarDecl _ -> false
    | _ -> true

  let process_par = function
    | Par (_,_,z,lc) -> 
      let ret = is_par_safe_to_convert z in
      let () = IFDEF DEBUG THEN print_endline ("Par can be vectorized: " ^ (string_of_bool ret)) ELSE () ENDIF in
      ret
    | _ as s -> raise (Internal_compiler_error ("Got a non par stmt for checking: " ^ (Dot.dot_stmt s)))

end 


module Convert =
struct
  open Language
  open Language
  open Consts
  open CFG
  module Array = Batteries.Array
  module List = Batteries.List
  module Int = Batteries.Int
  module Float = Batteries.Float
    
  exception Internal_compiler_error of string
  exception Error of string
  exception Ignore of string

  let runtime_vec = ref false
      
  let rec filter_shm = function
    | Plus (x,y,lc)| Minus (x,y,lc) | Times (x,y,lc) 
    | Div (x,y,lc) | Rshift (x,y,lc) | Lshift (x,y,lc)
    | Mod (x,y,lc) -> filter_shm x && filter_shm y
    | Brackets (x,lc) -> filter_shm x
    | Opposite (x,lc) -> filter_shm x
    | Abs (x,lc) -> filter_shm x
    | Constvector _ -> true 
    | Vector _ -> !runtime_vec (* Also account for the runtime vectors if the user asks for runtime vectorization *)
    | _ -> false

  (* Calculate the shuffle mask *)

  let process_vectors access_sizes f1 f2 = function
    | (Constvector (_,_,x,_) , Constvector (_,d,y,lc)) ->
      let ar = (Array.map2 (fun (Const(d,x,_)) (Const(t,y,_)) -> 
	Constantfolding.Constantpropogation.process (VConst(d,x)) (VConst(t,y)) f1 f2) x y) in
      let ar = Array.map (fun (VConst(d,x)) -> Const(d,x,lc)) ar in
      Constvector (None,d,ar,lc)
    | _ -> raise (Internal_compiler_error "")


  let rec calculate_shuffle_mask access_sizes lc = function
    | Plus (x,y,lc) ->
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.add Float.add (l,r)

    | Minus (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.sub Float.sub (l,r)

    | Times (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.mul Float.mul (l,r)

    | Div (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.div Float.div (l,r)

    | Pow (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.pow Float.pow (l,r)

    | Lshift (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes (lsl) (Constantfolding.Constantpropogation.lsd) (l,r)

    | Rshift (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes (lsr) (Constantfolding.Constantpropogation.lsd) (l,r)

    | Mod (x,y,lc) -> 
      let l = calculate_shuffle_mask access_sizes lc x in
      let r = calculate_shuffle_mask access_sizes lc y in
      process_vectors access_sizes Int.rem Float.modulo (l,r)


    | Brackets (x,lc) -> calculate_shuffle_mask access_sizes lc x

    | Cast _ -> raise (Internal_compiler_error "Cannot cast non const vector types yet!! ")

    | Opposite _ -> raise (Internal_compiler_error "Cannot do opposite vectors yet!! ")
    | Abs _ -> raise (Internal_compiler_error "Cannot do opposite vectors yet!! ")

    | ColonExpr _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a ColonExpr"))

    | Const _ ->raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a Const"))

    | AddrRef _-> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a AddrRef"))

    | VarRef _ -> raise (Ignore ((Reporting.get_line_and_column lc) ^ " erroroneously got a VarRef"))
    | Vector _ -> 
      if !runtime_vec then
	raise (Ignore ((Reporting.get_line_and_column lc) ^ " erroroneously got a Vector type index without runtime vectorization option"))
      else raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a VecRef"))
    | VecRef _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " erroroneously got a VecRef"))
    | Constvector (i1,d,ar,lc) as s -> s

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
    | Assign (x,_,lc,_) -> List.iter (fun x -> (match x with | AllTypedSymbol x -> symbol_table := x :: !symbol_table | _ -> ())) x
    | Block (x,_) -> List.iter (build_symbol_table symbol_table) x
    | CaseDef (x,lc) -> build_case_symbol_table symbol_table x
    | For (s,_,x,lc) | Par (s,_,x,lc) ->  build_symbol_table symbol_table x
    | Split (x,_) ->  build_symbol_table symbol_table x
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
    | Cast (x,v,lc) -> get_const lc v
    | Opposite (x,lc) -> 
      let r = get_const lc x in
      (match r with
	| Const (x,v,lc) -> 
	  let vr = int_of_string v in Const (x, (string_of_int (-vr)),lc)
	| _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " not of type const: " ^ (Dot.dot_simpleexpr s))))
    | _ as s -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " not of type const: " ^ (Dot.dot_simpleexpr s)))

  let get_access_size s e st = abs (((e - s)/st)) + 1

  let build_shuffle_mask s e st lc = 
    let shuffle_mask = 
      (try 
	 Array.init (get_access_size s e st) (fun i -> s)
       with
	 | _ as s -> print_endline (Reporting.get_line_and_column lc); raise s) in
    (* Now make the elements of the array correctly!! *)
    let counter = ref s in
    Array.map (fun x -> let ret = !counter in counter := !counter + st; ret) shuffle_mask

  let build_counter_mask s e =  Array.init (e-s+1) (fun i -> i)

  (* This is for the store side *)
  let build_inverse_shuffle_mask cmask sm lc = 
    let tot = ref cmask in
    let () = IFDEF DEBUG THEN print_endline "Counter mask" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN Array.iter (fun x -> print_endline ((string_of_int x) ^ ",")) !tot ELSE () ENDIF in
    let () = IFDEF DEBUG THEN print_endline "Shuffle mask" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN Array.iter (fun x -> print_endline ((string_of_int x) ^ ",")) sm ELSE () ENDIF in
    let () = Array.iter (fun i -> tot := (Array.filter (fun x ->  x <> i) !tot)) sm in 
    let () = IFDEF DEBUG THEN print_endline "Inverse mask" ELSE () ENDIF in
    let () = IFDEF DEBUG THEN Array.iter (fun x -> print_endline ((string_of_int x) ^ ",")) !tot ELSE () ENDIF in
    !tot
      
  let build_permute_mask tot first second lc = 
    let c1 = 0 in
    let c2 = Array.length first in
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

  (* TODO: Change this function to accept all types of simple expressions except for AddrRef just skip that one!! *)
  let rec get_index_to_conv lc index = function
    | VarRef (x,_) as s -> 
      let ret = (get_symbol index) = (get_symbol x) in
      let () = IFDEF DEBUG THEN print_endline ("Index " ^ (get_symbol index) ^ " = " ^ (get_symbol x)) ELSE () ENDIF in
      ret
    | Brackets (x,_) -> get_index_to_conv lc index x
    | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " cannot find the index in the Par index list"))

  let get_index_to_convert lc index counter = function
    | DimSpecExpr x -> 
      let ret = get_index_to_conv lc index x in
      let () = IFDEF DEBUG THEN print_endline (string_of_bool ret) ELSE () ENDIF in 
      ret

  let index_to_convert_to lc index = function
    | BracDim l -> List.findi (get_index_to_convert lc index) l
      
  let check_index lc index = function
    | BracDim l -> 
      let ret = List.filter (get_index_to_convert lc index 0) l in
      let () = IFDEF DEBUG THEN print_endline (" GOT list length : " ^ (string_of_int (List.length ret))) ELSE () ENDIF in
      ret

  let build_counter_mask s e =  Array.init (e-s+1) (fun i -> i)

end 
