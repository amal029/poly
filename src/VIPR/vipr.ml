module DataTypes = 
struct
  type t =
    | Int8 | Int16 | Int32 | Int64
    | Int8s | Int16s | Int32s | Int64s
    | Float8 | Float16 | Float32 | Float64
    | Filter of t list * t list
    | Aggregate of AccessPattern.tunablevariable list * AccessPattern.accesspattern list * t
    | None
    | Bool
    | Poly of string

  let getdata_size = function
    | Int8 -> 8
    | Int16 -> 16
    | Int32 -> 32
    | Int64 -> 64
    | Int8s -> 8
    | Int16s -> 16
    | Int32s -> 32
    | Int64s -> 64
    | Float8 -> 8
    | Float16 -> 16
    | Float32 -> 32
    | Float64 -> 64
    | _ -> raise (Error ("DataType not recognized"))

      
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

  (* Type Ranges *)
  let unsignedIntegral    = [Int8; Int16; Int32; Int64];;
  let signedIntegral      = [Int8s; Int16s; Int32s; Int64s];;
  let integral            = unsignedIntegral @ signedIntegral;;
  let floating            = [Float8; Float16; Float32; Float64];;
  let signed              = floating @ signedIntegral;;
  let numeric             = integral @ floating;;

  
end

type types = 
  | Ground of DataTypes.t
  | Aggregate of int list * DataTypes.t
  | Tile of int list * DataTypes.t

type literal = string

type op = 
  | POW
  | PLUS
  | MINUS
  | DIV
  | TIMES
  | MOD
  | EQEQ
  | EQ
  | LEQ
  | LT
  | GEQ
  | GT
  | RSHIFT
  | LSHIFT

type index =
  | StaticIndex of int list
  | DynamicIndex of expression list

and storage = 
  | Array of literal 
  | Variable of literal * groundType
  | Subarray of literal * types * types * index

and reference =
  | StaticArrayRef of literal * index
  | DynamicArrayRef of literal * index
  | VariableRef of literal
  | Constant of literal

and expression = 
  | Ref of reference
  | Binop of op * expression * expression
  | Unop of op * expression * expression
  | Brackets of expression
  | Cast of groundType * expression

and rExpression = 
  | LitTrue
  | LitFalse
  | RBinop of op * expression * expression
  | RBrackets of rExpression
  | And of rExpression * rExpression
  | Or of rExpression * rExpression
  | Not of rExpression

and statement = 
  | If of rExpression * statement * statement
  | For of storage * expression * rExpression * statement * statement
  | Par of storage * expression * rExpression * statement * statement
  | Declare of storage
  | DeclareFun of procedure
  | DeclareEntry of procedure
  | Assign of reference * expression
  | DeclareAndAssign of storage * expression
  | DeclareArrayConst of storage * expression
  | CallFun of literal * reference list * reference list
  | Block of statement list
  | Noop

and procedure =
  | Procedure of literal * storage list * storage list * statement

type ast =
  | Program of statement list 
