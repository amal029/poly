module DataTypes =
struct
  open Xml
  exception Error of string;;
  (* Debugging function *)
  type t =
    | Int8 | Int16 | Int32 | Int64
    | Int8s | Int16s | Int32s | Int64s
    | Float8 | Float16 | Float32 | Float64
    | Filter of t list * t list
    | Aggregate of t * int list
    | None
    | Bool
    | Poly of string

  let print_datatype = function
    | Int8 -> "Int8"
    | Int16 -> "Int16"
    | Int32 -> "Int32"
    | Int64 -> "Int64"
    | Int8s -> "Int8s"
    | Int16s -> "Int16s"
    | Int32s -> "Int32s"
    | Int64s -> "Int64s"
    | Float8 -> "Float8"
    | Float16 -> "Float16"
    | Float32 -> "Float32"
    | Float64 -> "Float64"
    | None -> "None"
    | Poly x -> x
    | Bool -> "Bool"
    | _ -> raise (Error ("DataType not recognized"))

      
  let get_size x = Element("Size",[],[PCData (string_of_int x)])

  let datatype_xml = function
    | Aggregate (x,y) -> 
      let primitive = Element ("Type",[],[Element((print_datatype x),[],[])]) in
      let sizelist = Element ("SizeList",[],(List.map (fun x -> get_size x) y)) in
      Element("Type",[],[Element("Aggregate",[],([primitive;sizelist]))])
    |  _ as s -> Element("Type",[],[Element((print_datatype s),[],[])])

end

module AndrewLang = 

struct 


  type symbol = Symbol of string

  type index = int

  type value = string

  type simpleExpr = 
    | Plus of simpleExpr * simpleExpr
    | Minus of simpleExpr * simpleExpr
    | Times of simpleExpr * simpleExpr
    | Div of simpleExpr * simpleExpr
    | Pow of simpleExpr * simpleExpr
    | Const of DataTypes.t * value
    | Ref of rsymbol
    | Brackets of simpleExpr
    | Cast of DataTypes.t * simpleExpr
    | Negate of simpleExpr

  and addressedSymbol =
      AddressedSymbol of symbol * comprehension

  and typedSymbol =
    | TypedSymbol of DataTypes.t * symbol (* Type Symbol *)

  and addresstypedsymbol = 
    | TypedAdressedSymbol of DataTypes.t * addressedSymbol

  (* This is the colon expr *)
  (*
    NOTE:
    In case of var T[10] --> this will ne 10 * None * None
    In case of for i in 0:10 --> this will be 0 * 10 * 1
  *)
  and dimspec = 
    | VarDimSpec of simpleExpr * simpleExpr option * simpleExpr option

  and comprehension =
    | Comprehension of dimspec list

  and rsymbol = 
    | RSym of symbol
    | RASym of addressedSymbol

  type callinputlist = rsymbol list
  type calloutputlist = rsymbol list
  type functionCall = Call of symbol * callinputlist * calloutputlist

  type relExpr = 
    | LessThan of simpleExpr * simpleExpr
    | LessThanEqual of simpleExpr * simpleExpr
    | GreaterThan of simpleExpr * simpleExpr
    | GreaterThanEqual of simpleExpr * simpleExpr
    | EqualTo of simpleExpr * simpleExpr

  type stmt = 
    | Assign of rsymbol * simpleExpr (*a=10*)
    | VarDecl of typedSymbol (*create *)
    | DeclAssign of typedSymbol * simpleExpr
    | AggregateDeclAssign of typedSymbol * value list
    | CaseDef of case
    | FCall of functionCall
    | Escape of string
    | Par of symbol * dimspec * block
    | For of symbol * dimspec * block
  and case =
    | Case of caseClause list * otherwise
  and caseClause = 
    | Clause of relExpr * block
  and otherwise = 
    | Otherwise of block
  and block = stmt list

(* Top level  *)
  type inputlist = typedSymbol list
  type outputlist = typedSymbol list
  type filter = Function of symbol * inputlist * outputlist * block

  type toplevelStmt = 
    | Def of filter
    | DefMain of filter
    | TopEscape of string

  type ast = 
    | Program of toplevelStmt list

end
