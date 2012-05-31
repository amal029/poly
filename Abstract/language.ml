module AccessPattern = 
struct
  type tunablevariable = 
    | String of string
    | Integer of int
  type accesspattern = 
    | Const
    | Simple
    | Map
    | Block  of tunablevariable list
    | Border of tunablevariable list
    | Window of tunablevariable list
    | Stride of tunablevariable list
    | Tiles  of tunablevariable list
end

module DataTypes =
struct
  exception Error of string;;
  (* Debugging function *)
  type t =
    | Int8 | Int16 | Int32 | Int64
    | Int8s | Int16s | Int32s | Int64s
    | Float8 | Float16 | Float32 | Float64
    | Filter of t list * t list
    | Aggregate of AccessPattern.tunablevariable list * AccessPattern.accesspattern list * t
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

  (* Type Ranges *)
  let unsignedIntegral    = [Int8; Int16; Int32; Int64];;
  let signedIntegral      = [Int8s; Int16s; Int32s; Int64s];;
  let integral            = unsignedIntegral @ signedIntegral;;
  let floating            = [Float8; Float16; Float32; Float64];;
  let signed              = floating @ signedIntegral;;
  let numeric             = integral @ floating;;
end
;;

module Language =
struct
  (* the overall ast *)
  type symbol =
      Symbol of string

  type index = int


  type value = string

  type simpleExpr = 
    | TStar
    | TStarStar
    | Plus of simpleExpr * simpleExpr
    | Minus of simpleExpr * simpleExpr
    | Times of simpleExpr * simpleExpr
    | Div of simpleExpr * simpleExpr
    | Pow of simpleExpr * simpleExpr
    | Const of DataTypes.t * value
    | VarRef of symbol
    | AddrRef of addressedSymbol
    | Brackets of simpleExpr
    | Cast of DataTypes.t * simpleExpr
    | ColonExpr of simpleExpr * simpleExpr * simpleExpr
    | Opposite of simpleExpr
  and addressedSymbol =
      AddressedSymbol of symbol * angledim list * dimspec list
  and dimSpecExpr =
    | DimSpecExpr of simpleExpr
  and angledim =
    | AngleDimExpr of dimSpecExpr
  and dimspec =
    | BracDim of dimSpecExpr list
  and typedSymbol =
    | SimTypedSymbol of DataTypes.t * symbol (* Type Symbol *)
    | ComTypedSymbol of DataTypes.t * addressedSymbol

  type callArgument = 
    | CallAddrressedArgument of addressedSymbol
    | CallSymbolArgument of symbol
  and filterCall = Call of symbol * callArgument list

  type relExpr = 
    | LessThan of simpleExpr * simpleExpr
    | LessThanEqual of simpleExpr * simpleExpr
    | GreaterThan of simpleExpr * simpleExpr
    | GreaterThanEqual of simpleExpr * simpleExpr
    | EqualTo of simpleExpr * simpleExpr

  type allsym =
    | AllAddressedSymbol of addressedSymbol
    | AllSymbol of symbol
    | AllTypedSymbol of typedSymbol


  type stmt = 
    | Assign of allsym list * expr (*a=10*)
    | VarDecl of typedSymbol (*create *)
    | CaseDef of case
    | Escape of string
    | Block of stmt list
    | Par of symbol * simpleExpr * stmt
    | For of symbol * simpleExpr * stmt
    | Noop
  and expr =
    | FCall of filterCall
    | SimExpr of simpleExpr
  and case =
    | Case of caseClause list * otherwise
  and caseClause = 
    | Clause of relExpr * stmt
  and otherwise = 
    | Otherwise of stmt

  (* Top level  *)
  type filter = Filter of symbol * typedSymbol list * typedSymbol list * stmt

  type toplevelStmt = 
    | Def of filter
    | DefMain of filter
    | TopEscape of string

  type ast = 
    | Program of toplevelStmt list
end


(* This structure defines the control flow graph at the filter level *)
(* Useful for single static assignment of filters, checking if there is
   recursion and constant propogation
*)
module FCFG =
struct
  open Language
  type fcfg = 
    | Node of stmt * filter * fcfg list
end

module CFG =
struct
  open Language
  type cfg =
    | Startnode of stmt * cfg (* When entering a complex statement *)
    | Squarenode of stmt * cfg (* this is the internal cfg within the filter *)
    | Conditionalnode of relExpr * cfg * cfg (* a conditional has true and false outputs *)
    | Endnode of stmt * cfg * int (* can have multiple incoming nodes, but only one outgoing node *)
    | Backnode of cfg ref (* For loops (*pun intended*) *)
    | Empty
  and topnode = 
    | Topnode of stmt * string * cfg list (* These are my statements within a filter and its name *)
    | Null
  type filternode = 
    | Filternode of topnode * filternode list (* These are the connections to other filters' topnodes *)
end

module Consts = 
struct 
  type consts = 
    | VConst of DataTypes.t * string
    | Top of DataTypes.t
end


