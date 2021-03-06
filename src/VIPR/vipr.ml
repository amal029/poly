type types = 
  | Ground of Language.DataTypes.t
  | Aggregate of int list * Language.DataTypes.t
  | Tile of int list * Language.DataTypes.t * int list

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
  | OPP
  | ABS

let op_print = function
  | POW -> "pow"
  | PLUS -> "plus"
  | MINUS -> "minus"
  | DIV -> "div"
  | TIMES -> "times"
  | MOD -> "mod"
  | EQEQ -> "eqeq"
  | EQ -> "eq"
  | LEQ -> "leq"
  | LT -> "lt"
  | GEQ -> "geq"
  | GT -> "gt"
  | RSHIFT -> "rshift"
  | LSHIFT -> "lshift"
  | OPP -> "opp"
  | ABS -> "abs"


type index =
  | StaticIndex of int list
  | DynamicIndex of expression list

and storage = 
  | Array of literal * types
  | Variable of literal * Language.DataTypes.t
  | Subarray of literal * types * index

and reference =
  | StaticArrayRef of literal * index
  | DynamicArrayRef of literal * index
  | VariableRef of literal
  | Constant of literal

and expression = 
  | Ref of reference
  | Binop of op * expression * expression
  | Unop of op * expression
  | Brackets of expression
  | RefL of literal list
  | Cast of Language.DataTypes.t * expression

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
  | DeclareArrayConst of storage * literal list
  (* This means one can pass constants as asguments, then how do we get the type of this constant?? *)
  | CallFun of literal * reference list * reference list
  | Block of statement list
  | Noop

and procedure =
  | Procedure of literal * storage list * storage list * statement

type ast =
  | Program of statement list 
