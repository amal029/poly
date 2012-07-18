(*

  Author: Avinash Malik

  Purpose : This is the decompiler that produces code for AndrewLang
  Note: That this compiler goes from my ast to andrew's ast not directly
  from CFG to andrews ast!!

  Date: Thu May 31 16:45:15 IST 2012

*)

open Language
open Language
open CFG

exception Internal_compiler_error of string

let get_andrew_primitive_datatype = function
  | DataTypes.Int8-> AndrewLang.DataTypes.Int8 
  | DataTypes.Int16 -> AndrewLang.DataTypes.Int16
  | DataTypes.Int32 -> AndrewLang.DataTypes.Int32
  | DataTypes.Int64-> AndrewLang.DataTypes.Int64
  | DataTypes.Int8s -> AndrewLang.DataTypes.Int8s
  | DataTypes.Int16s -> AndrewLang.DataTypes.Int16s
  | DataTypes.Int32s -> AndrewLang.DataTypes.Int32s
  | DataTypes.Int64s-> AndrewLang.DataTypes.Int64s
  | DataTypes.Float8 -> AndrewLang.DataTypes.Float8
  | DataTypes.Float16 -> AndrewLang.DataTypes.Float16
  | DataTypes.Float32 -> AndrewLang.DataTypes.Float32
  | DataTypes.Float64 -> AndrewLang.DataTypes.Float64
  | _ as s -> 
    (* Debugging *)
    let () = IFDEF DEBUG THEN print_endline (DataTypes.print_datatype s) ELSE () ENDIF in
    raise (Internal_compiler_error "Wrong datatype detected")

let get_andrew_symbol = function
  | Symbol (x,_) -> AndrewLang.AndrewLang.Symbol x 



let get_andrew_comprehension x = AndrewLang.AndrewLang.Comprehension x

let rec get_andrew_simple_expr = function
  | Plus (x,y,_) -> AndrewLang.AndrewLang.Plus ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Minus (x,y,_) -> AndrewLang.AndrewLang.Minus ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Times (x,y,_) -> AndrewLang.AndrewLang.Times ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Pow (x,y,_) -> AndrewLang.AndrewLang.Pow ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Lshift (x,y,_) -> AndrewLang.AndrewLang.Lshift ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Rshift (x,y,_) -> AndrewLang.AndrewLang.Rshift ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Div (x,y,_) -> AndrewLang.AndrewLang.Div ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Mod (x,y,_) -> AndrewLang.AndrewLang.Mod ((get_andrew_simple_expr x), (get_andrew_simple_expr y))
  | Const (x,y,_) -> AndrewLang.AndrewLang.Const ((get_andrew_primitive_datatype x),y)
  | VarRef (x,_) -> AndrewLang.AndrewLang.Ref (get_andrew_rsymbol x)
  | AddrRef (x,_) -> AndrewLang.AndrewLang.Ref (get_andrew_add_rsymbol x)
  | Cast (x,y,_) -> AndrewLang.AndrewLang.Cast ((get_andrew_primitive_datatype x),(get_andrew_simple_expr y))
  | Brackets (x,_) -> AndrewLang.AndrewLang.Brackets (get_andrew_simple_expr x)
  | Opposite (x,_) -> AndrewLang.AndrewLang.Negate (get_andrew_simple_expr x)
  | ColonExpr (x,y,z,_) -> 
    let d = (get_andrew_vardim_spec (get_andrew_simple_expr x) (Some (get_andrew_simple_expr y)) (Some (get_andrew_simple_expr z))) in
    AndrewLang.AndrewLang.ColonExpr d
  | TStar | TStarStar -> raise (Internal_compiler_error "Decompiler poly to Andrew Lang erroneously hits a TStar/TStarStar")

and get_andrew_vardim_spec x y z = AndrewLang.AndrewLang.VarDimSpec (x,y,z)

and get_andrew_rsymbol = function
  | Symbol (x,_) as s -> AndrewLang.AndrewLang.RSym (get_andrew_symbol s)

and get_andrew_add_rsymbol = function
  | AddressedSymbol _ as s -> AndrewLang.AndrewLang.RASym (get_andrew_addressed_symbol s)

and decompile_dimspecexpr = function
  | DimSpecExpr x -> get_andrew_simple_expr  x

and decompile_dimspec = function
  | BracDim x -> List.map (fun x -> decompile_dimspecexpr x) x

and decompile_addressed_symbol_dimensions = function
  | AddressedSymbol (_,_,x,_) -> List.flatten (List.map (fun x -> decompile_dimspec x) x)

and get_andrew_addressed_symbol = function
  | AddressedSymbol (x,y,z,_) as s -> 
    (* The size_list is of andrew_simple_expr *)
    let ll = decompile_addressed_symbol_dimensions s in
    (* First we need to make the vardim spec type and then make the comprehensions *)
    let ll = List.map (fun x -> get_andrew_vardim_spec x None None) ll in
    AndrewLang.AndrewLang.AddressedSymbol (get_andrew_symbol x, get_andrew_comprehension ll)

let get_andrew_typed_symbol x y = AndrewLang.AndrewLang.TypedSymbol ((get_andrew_primitive_datatype x),(get_andrew_symbol y))

let get_dims_of_consts = function
  | AndrewLang.AndrewLang.Const (x,y) -> (int_of_string y)
  | _ -> raise (Internal_compiler_error "Decompiler poly to AndrewLang: Arraydeclaration contains non consts for dimension specifiers")

let get_andrew_typed_address_symbol x y = 
  let primitive_type = (get_andrew_primitive_datatype x) in 
    (* The size_list is of andrew_simple_expr *)
  let size_list = (decompile_addressed_symbol_dimensions y) in
  (* Need to check that this size_list is always const type and get the
     string value as int from there *)
  let size_list = List.map (fun x -> get_dims_of_consts x) size_list in
  AndrewLang.AndrewLang.TypedAddressedSymbol ((AndrewLang.DataTypes.Aggregate (primitive_type, size_list)),(get_andrew_addressed_symbol y))

let decompile_typedsymbol = function
  | SimTypedSymbol (x,y,_) -> get_andrew_typed_symbol x y
  | ComTypedSymbol (x,y,_) -> get_andrew_typed_address_symbol x y

let rec get_andrew_relexpr = function
  | LessThan (x,y,_) -> AndrewLang.AndrewLang.LessThan ((get_andrew_simple_expr x),(get_andrew_simple_expr y))
  | LessThanEqual (x,y,_) -> AndrewLang.AndrewLang.LessThanEqual ((get_andrew_simple_expr x),(get_andrew_simple_expr y))
  | GreaterThanEqual (x,y,_) -> AndrewLang.AndrewLang.GreaterThanEqual ((get_andrew_simple_expr x),(get_andrew_simple_expr y))
  | GreaterThan (x,y,_) -> AndrewLang.AndrewLang.GreaterThan ((get_andrew_simple_expr x),(get_andrew_simple_expr y))
  | EqualTo (x,y,_) -> AndrewLang.AndrewLang.EqualTo ((get_andrew_simple_expr x),(get_andrew_simple_expr y))
  | Rackets (x,_) -> AndrewLang.AndrewLang.Rackets(get_andrew_relexpr x)
  | And (x,y,_) -> AndrewLang.AndrewLang.And (get_andrew_relexpr x, get_andrew_relexpr y)
  | Or (x,y,_) -> AndrewLang.AndrewLang.Or (get_andrew_relexpr x, get_andrew_relexpr y)


let get_andrew_call_list = function
  | CallAddrressedArgument x -> get_andrew_add_rsymbol x
  | CallSymbolArgument x -> get_andrew_rsymbol x

let get_andrew_allsym_list = function
  | AllTypedSymbol x -> 
    (* In this case we just extract the symbols from the typed notations!! *)
    let tsym = (decompile_typedsymbol x) in
    (match tsym with
      | AndrewLang.AndrewLang.TypedAddressedSymbol (_,x) -> (AndrewLang.AndrewLang.RASym x)
      | AndrewLang.AndrewLang.TypedSymbol (_,x) -> (AndrewLang.AndrewLang.RSym x))
  | AllAddressedSymbol x -> (get_andrew_add_rsymbol x)
  | AllSymbol x -> (get_andrew_rsymbol x)

let get_andrew_function_call outputs = function
  | Call (x,inputs,_)  -> AndrewLang.AndrewLang.Call ((get_andrew_symbol x),
						    (List.map (fun x -> get_andrew_call_list x) inputs),
						    (List.map (fun x -> get_andrew_allsym_list x) outputs))

let get_andrew_assign_stmt expr = function
  | AllTypedSymbol x -> AndrewLang.AndrewLang.DeclAssign ((decompile_typedsymbol x), expr)
  | AllAddressedSymbol x -> AndrewLang.AndrewLang.Assign ((get_andrew_add_rsymbol x), expr)
  | AllSymbol x -> AndrewLang.AndrewLang.Assign ((get_andrew_rsymbol x), expr)

let decompile_simple_assign outputs expr = 
  List.map (fun x -> get_andrew_assign_stmt (get_andrew_simple_expr expr) x) outputs 

let rec get_andrew_stmt = function
  | VarDecl (x,_) ->  [AndrewLang.AndrewLang.VarDecl (decompile_typedsymbol x)]
  | Escape (x,_) -> [AndrewLang.AndrewLang.Escape x]
  | Block (x,_) -> List.flatten (List.map (fun x -> get_andrew_stmt x) x)
  | Par (x,y,z,_) -> 
    let d = (match (get_andrew_simple_expr y) with AndrewLang.AndrewLang.ColonExpr x -> x | _ -> raise (Internal_compiler_error "Not found a colon expr "))  in
    [AndrewLang.AndrewLang.Par((get_andrew_symbol x),d,(get_andrew_stmt z))]
  | For (x,y,z,_) -> 
    let d = (match (get_andrew_simple_expr y) with AndrewLang.AndrewLang.ColonExpr x -> x | _ -> raise (Internal_compiler_error "Not found a colon expr "))  in
    [AndrewLang.AndrewLang.For((get_andrew_symbol x),d,(get_andrew_stmt z))]
  | Noop -> []
  | CaseDef (x,_) -> [AndrewLang.AndrewLang.CaseDef (get_andrew_case x)]
  | Assign (x,y,_) -> (match y with | SimExpr s -> decompile_simple_assign x s | FCall (f,e) -> [AndrewLang.AndrewLang.FCall ((get_andrew_function_call x f),e)])

and get_andrew_case = function
  | Case (x,y,lc) -> AndrewLang.AndrewLang.Case ((List.map (fun x -> get_andrew_clause x) x) , (get_andrew_otherwise y))

and get_andrew_clause = function
  | Clause (x,y,_) -> AndrewLang.AndrewLang.Clause ((get_andrew_relexpr x),(get_andrew_stmt y))

and get_andrew_otherwise = function
  | Otherwise (x,_) -> AndrewLang.AndrewLang.Otherwise (get_andrew_stmt x)

let get_andrew_filter = function
  | Filter (x,y,z,t) -> 
    let () = IFDEF DEBUG THEN print_endline ("Filter: " ^ (Dot.get_symbol x)) ELSE () ENDIF in
    AndrewLang.AndrewLang.Function ((get_andrew_symbol x),(List.map (fun x -> decompile_typedsymbol x)y),
						      (List.map (fun x -> decompile_typedsymbol x) z), get_andrew_stmt t)

let get_andrew_toplevelstmt = function
  | Def (x,_,_) -> 
    AndrewLang.AndrewLang.Def (get_andrew_filter x)
  | DefMain (x,_,_) -> AndrewLang.AndrewLang.DefMain (get_andrew_filter x)
  | _ -> raise (Internal_compiler_error "I do not allow topescapes, they suck!!")

let get_andrew_program = function
  | Program x -> AndrewLang.AndrewLang.Program (List.map (fun x -> get_andrew_toplevelstmt x) x)
