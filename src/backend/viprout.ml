open Vipr

module Buffer = Batteries.Buffer
module List = Batteries.List

exception Internal_compiler_error of string

(* This file pretty prints the lisp style output that is VIPR *)

let output_poly_types buffer pt = 
  Buffer.add_string buffer (Language.DataTypes.print_datatype pt)

let output_vipr_types buffer = function
  | Ground pt -> 
    let () = Buffer.add_string buffer "(Ground" in
    let () = output_poly_types buffer pt in
    Buffer.add_string buffer ")"

  | Aggregate (il,pt) -> 
    let () = Buffer.add_string buffer "(Aggregate" in
    let () = Buffer.add_string buffer "[" in
    let length = List.length il in
    let () = List.iteri (fun i x -> Buffer.add_string buffer (string_of_int x);
      (if i < (length-1) then Buffer.add_string buffer ", " 
       else Buffer.add_string buffer "]")) il in
    let () = output_poly_types buffer pt in
    Buffer.add_string buffer ")"

  | Tile _ -> raise (Internal_compiler_error "Tiles currently not supported!!")

let output_vipr_storage buffer = function
  | Array (l,t) -> 
    let () = Buffer.add_string buffer ("(Array\"" ^ l ^ "\"") in
    let () = output_vipr_types buffer t in
    Buffer.add_string buffer ")"

  | Variable (l,pt) -> 
    let () = Buffer.add_string buffer ("(Variable\"" ^ l ^ "\"") in
    let () = output_poly_types buffer pt in
    Buffer.add_string buffer ")"

  | Subarray _ -> raise (Internal_compiler_error "SubArray currently not supported!!")

let output_vipr_operator buffer = function
  | POW -> Buffer.add_string buffer "\"^\""
  | MINUS -> Buffer.add_string buffer "\"-\""
  | PLUS -> Buffer.add_string buffer "\"+\""
  | TIMES -> Buffer.add_string buffer "\"*\""
  | DIV -> Buffer.add_string buffer "\"/\""
  | GEQ -> Buffer.add_string buffer "\">=\""
  | GT -> Buffer.add_string buffer "\">\""
  | LT -> Buffer.add_string buffer "\"<\""
  | LEQ -> Buffer.add_string buffer "\"<=\""
  | EQEQ -> Buffer.add_string buffer "\"==\""
  | LSHIFT -> Buffer.add_string buffer "\"<<\""
  | RSHIFT -> Buffer.add_string buffer "\">>\""
  | OPP -> Buffer.add_string buffer "\"-\""
  | ABS -> Buffer.add_string buffer "\"abs\""
  | EQ -> Buffer.add_string buffer "\"=\""
  | MOD -> Buffer.add_string buffer "\"%\""


let rec output_vipr_expression buffer = function
  | Ref x -> 
    let () = Buffer.add_string buffer "(Ref" in
    let () = output_vipr_reference buffer x in
    Buffer.add_string buffer ")"

  | RefL l ->
    let () = Buffer.add_string buffer "(RefL[" in
    let length = List.length l in
    let () = List.iteri (fun i x -> Buffer.add_string buffer ("\""^x^"\"");
      (if i < (length-1) then Buffer.add_string buffer ", "
       else Buffer.add_string buffer "]") ) l in
    Buffer.add_string buffer ")"

  | Binop (op,exp1,exp2) -> 
    let () = Buffer.add_string buffer "(Binop" in
    let () = output_vipr_operator buffer op in
    let () = output_vipr_expression buffer exp1 in
    let () = output_vipr_expression buffer exp2 in
    Buffer.add_string buffer ")"
      
  | Unop (op, exp) -> 
    let () = Buffer.add_string buffer "(Unop" in
    let () = output_vipr_operator buffer op in
    let () = output_vipr_expression buffer exp in
    Buffer.add_string buffer ")"

  | Brackets x -> 
    let () = Buffer.add_string buffer "(Brackets" in
    let () = output_vipr_expression buffer x in
    Buffer.add_string buffer ")"

  | Cast (pt,x) -> 
    let () = Buffer.add_string buffer "(Cast" in
    let () = output_poly_types buffer pt in
    let () = output_vipr_expression buffer x in
    Buffer.add_string buffer ")"

and output_vipr_reference buffer = function
  | StaticArrayRef (l,index) ->
    let () = Buffer.add_string buffer ("(StaticArrayRef\"" ^ l ^ "\"") in
    let () = output_vipr_index buffer index in
    Buffer.add_string buffer ")"

  | DynamicArrayRef (l,index) -> 
    let () = Buffer.add_string buffer ("(DynamicArrayRef\"" ^ l ^ "\"") in
    let () = output_vipr_index buffer index in
    Buffer.add_string buffer ")"

  | VariableRef l -> 
    Buffer.add_string buffer ("(VariableRef\"" ^ l ^ "\")")

  | Constant l ->
    Buffer.add_string buffer ("(Constant\"" ^ l ^ "\")")

and output_vipr_index buffer = function
  | StaticIndex il -> 
    let () = Buffer.add_string buffer "(StaticIndex[" in
    let length = List.length il in
    let () = List.iteri (fun i x -> Buffer.add_string buffer (string_of_int x);
      (if i < (length-1) then Buffer.add_string buffer ", " 
       else Buffer.add_string buffer "]")) il in
    Buffer.add_string buffer ")"

  | DynamicIndex el ->
    let () = Buffer.add_string buffer "(DynamicIndex[" in
    let length = List.length el in
    let () = List.iteri (fun i x -> output_vipr_expression buffer x;
      (if i < (length-1) then Buffer.add_string buffer ", " 
       else Buffer.add_string buffer "]")) el in
    Buffer.add_string buffer ")"

let rec output_vipr_rexpression buffer = function
  | LitTrue -> Buffer.add_string buffer "(LitTrue)"
  | LitFalse -> Buffer.add_string buffer "(LitFalse)"
  | RBinop (op,exp1,exp2) ->
    let () = Buffer.add_string buffer "(RBinop" in
    let () = output_vipr_operator buffer op in
    let () = output_vipr_expression buffer exp1 in
    let () = output_vipr_expression buffer exp2 in
    Buffer.add_string buffer ")"
  | RBrackets x -> 
    let () = Buffer.add_string buffer "(RBrackets" in
    let () = output_vipr_rexpression buffer x in
    Buffer.add_string buffer ")"
  | And (r1,r2) -> 
    let () = Buffer.add_string buffer "(And" in
    let () = output_vipr_rexpression buffer r1 in
    let () = output_vipr_rexpression buffer r2 in
    Buffer.add_string buffer ")"
  | Or (r1,r2) -> 
    let () = Buffer.add_string buffer "(Or" in
    let () = output_vipr_rexpression buffer r1 in
    let () = output_vipr_rexpression buffer r2 in
    Buffer.add_string buffer ")"
  | Not x -> 
    let () = Buffer.add_string buffer "(Not" in
    let () = output_vipr_rexpression buffer x in
    Buffer.add_string buffer ")"


let rec output_vipr_statement buffer = function
  | DeclareFun x -> 
    let () = Buffer.add_string buffer "(DeclareFun" in
    let () = output_vipr_procedure buffer x in
    Buffer.add_string buffer ")"

  | DeclareEntry x -> 
    let () = Buffer.add_string buffer "(DeclareEntry" in
    let () = output_vipr_procedure buffer x in
    Buffer.add_string buffer ")"

  | Declare st -> 
    let () = Buffer.add_string buffer "(Declare" in
    let () = output_vipr_storage buffer st in
    Buffer.add_string buffer ")"

  | Assign (re,exp) -> 
    let () = Buffer.add_string buffer "(Assign" in
    let () = output_vipr_reference buffer re in
    let () = output_vipr_expression buffer exp in
    Buffer.add_string buffer ")"

  | DeclareAndAssign (st,exp) -> 
    let () = Buffer.add_string buffer "(DeclareAndAssign" in
    let () = output_vipr_storage buffer st in
    let () = output_vipr_expression buffer exp in
    Buffer.add_string buffer ")"

  | Block sl -> 
    let () = Buffer.add_string buffer "(Block[" in
    let length = List.length sl in
    let () = List.iteri (fun i x -> output_vipr_statement buffer x;
      (if i < (length-1) then Buffer.add_string buffer ", "
       else Buffer.add_string buffer "]")) sl in
    Buffer.add_string buffer ")"

  | CallFun (l,i,o) -> 
    let () = Buffer.add_string buffer "(CallFun[" in
    let length = List.length i in
    let () = List.iteri (fun i x -> output_vipr_reference buffer x;
      (if i < (length-1) then Buffer.add_string buffer ", "
       else Buffer.add_string buffer "]")) i in
    let () = Buffer.add_string buffer "[" in
    let length = List.length o in
    let () = List.iteri (fun i x -> output_vipr_reference buffer x;
      (if i < (length-1) then Buffer.add_string buffer ", "
       else Buffer.add_string buffer "]")) o in
    Buffer.add_string buffer ")"

  | Noop -> Buffer.add_string buffer "(Noop)"

  | If (st,s1,s2) -> 
    let () = Buffer.add_string buffer "(If" in
    let () = output_vipr_rexpression buffer st in
    let () = output_vipr_statement buffer s1 in
    let () = output_vipr_statement buffer s2 in
    Buffer.add_string buffer ")"

  | For (st,exp,r,s1,s2) -> 
    let () = Buffer.add_string buffer "(For" in
    let () = output_vipr_storage buffer st in
    let () = output_vipr_expression buffer exp in
    let () = output_vipr_rexpression buffer r in
    let () = output_vipr_statement buffer s1 in
    let () = output_vipr_statement buffer s2 in
    Buffer.add_string buffer ")"

  | Par (st,exp,r,s1,s2) ->
    let () = Buffer.add_string buffer "(Par" in
    let () = output_vipr_storage buffer st in
    let () = output_vipr_expression buffer exp in
    let () = output_vipr_rexpression buffer r in
    let () = output_vipr_statement buffer s1 in
    let () = output_vipr_statement buffer s2 in
    Buffer.add_string buffer ")"

and output_vipr_procedure buffer = function
  | Procedure (l,i,o,st) -> 
    let () = Buffer.add_string buffer "(Procedure" in
    let () = Buffer.add_string buffer ("\""^l^"\"[") in
    let length = List.length i in
    let () = List.iteri (fun it i -> output_vipr_storage buffer i;
      (if it < (length-1) then Buffer.add_string buffer ", ")) i in
    let () = Buffer.add_string buffer "][" in
    let length = List.length o in
    let () = List.iteri (fun i o -> output_vipr_storage buffer o;
      (if i < (length-1) then Buffer.add_string buffer ", ")) o in
    let () = Buffer.add_string buffer "]" in
    let () = output_vipr_statement buffer st in
    Buffer.add_string buffer ")"

let output_vipr_program buffer = function
  | Program x -> 
    let () = Buffer.add_string buffer "Program [" in 
    let length = List.length x in
    let () = List.iteri (fun i x -> output_vipr_statement buffer x;
      (if i < (length-1) then Buffer.add_string buffer ", ")) x in
    Buffer.add_string buffer "]\n"

let output out node = 
  let buffer = Buffer.create 10000 in
  let () = output_vipr_program buffer node in
  (* Just write to the output stream provided *)
  Buffer.print out buffer
    
