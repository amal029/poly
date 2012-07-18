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

  let cmp_datatype (l,r) = 
    if List.exists (fun x -> x = l) unsignedIntegral && List.exists (fun x -> x = r) signedIntegral then
      "sext"
    else if List.exists (fun x -> x = l) unsignedIntegral && List.exists (fun x -> x = r) floating then
      "uitofp"
    else if List.exists (fun x -> x = l) signedIntegral && List.exists (fun x -> x = r) floating then
      "sitofp"
    else if List.exists (fun x -> x = l) signedIntegral && List.exists (fun x -> x = r) unsignedIntegral then
      "trunc"
    else if List.exists (fun x -> x = l) floating && List.exists (fun x -> x = r) unsignedIntegral then
      "fptoui"
    else if List.exists (fun x -> x = l) floating && List.exists (fun x -> x = r) signedIntegral then
      "fptosi"
    (* Now rest of the crappy ones *)
    else if (l = r) then "="
    else 
      begin
	match (l,r) with
	  | (Int8,_) -> "sext"
	  | (_,Int8) -> "trunc"
	  | (Int16, Int32) | (Int16,Int64) -> "zext"
	  | (Int32, Int16) | (Int64,Int16) -> "trunc"
	  | (Int32,Int64) -> "zext"
	  | (Int64,Int32) -> "trunc"
	  | (Int8s,_) -> "sext"
	  | (_,Int8s) -> "trunc"
	  | (Int16s,Int32s) | (Int16s,Int64s) -> "sext"
	  | (Int32s,Int16s) | (Int64s,Int16s) -> "trunc"
	  | (Int32s,Int64s) -> "sext"
	  | (Int64s,Int32s) -> "trunc"
	  | (Float8,_) -> "fpext"
	  | (_,Float8) -> "fptrunc"
	  | (Float16, Float32) | (Float16,Float64) -> "fpext"
	  | (Float32, Float16) | (Float64,Float16) -> "fptrunc"
	  | (Float32,Float64) -> "fpext"
	  | (Float64,Float32) -> "fptrunc"
      end
      
end
;;


(* FIXME: All the ast nodes need to be attached with line and column
   number information, which changes everything else as well.*)
module Language =
struct


  (* the overall ast *)
  type line = int
  type column = int

  type symbol =
      Symbol of string * (line * column)

  type index = int


  type value = string

  type simpleExpr = 
    | TStar
    | TStarStar
    | Plus of simpleExpr * simpleExpr * (line * column)
    | Minus of simpleExpr * simpleExpr * (line * column)
    | Times of simpleExpr * simpleExpr * (line * column)
    | Div of simpleExpr * simpleExpr * (line * column)
    | Mod of simpleExpr * simpleExpr * (line * column)
    | Pow of simpleExpr * simpleExpr * (line * column)
    | Rshift of simpleExpr * simpleExpr * (line * column)
    | Lshift of simpleExpr * simpleExpr * (line * column)
    | Const of DataTypes.t * value * (line * column)
    | VarRef of symbol * (line * column)
    | AddrRef of addressedSymbol * (line * column)
    | Brackets of simpleExpr * (line * column)
    | Cast of DataTypes.t * simpleExpr  * (line * column)
    | ColonExpr of simpleExpr * simpleExpr * simpleExpr * (line * column)
    | Opposite of simpleExpr * (line * column)
  and addressedSymbol =
      AddressedSymbol of symbol * angledim list * dimspec list * (line * column)
  and dimSpecExpr =
    | DimSpecExpr of simpleExpr
  and angledim =
    | AngleDimExpr of dimSpecExpr
  and dimspec =
    | BracDim of dimSpecExpr list
  and typedSymbol =
    | SimTypedSymbol of DataTypes.t * symbol * (line * column) (* Type Symbol *)
    | ComTypedSymbol of DataTypes.t * addressedSymbol * (line * column)

  type callArgument = 
    | CallAddrressedArgument of addressedSymbol
    | CallSymbolArgument of symbol

  and filterCall = Call of symbol * callArgument list * (line * column)

  type relExpr = 
    | LessThan of simpleExpr * simpleExpr * (line * column)
    | LessThanEqual of simpleExpr * simpleExpr * (line * column)
    | GreaterThan of simpleExpr * simpleExpr * (line * column)
    | GreaterThanEqual of simpleExpr * simpleExpr * (line * column)
    | EqualTo of simpleExpr * simpleExpr * (line * column)
    | And of relExpr * relExpr * (line * column)
    | Or of relExpr * relExpr * (line * column)
    | Rackets of relExpr * (line * column)

  type allsym =
    | AllAddressedSymbol of addressedSymbol
    | AllSymbol of symbol
    | AllTypedSymbol of typedSymbol


  type stmt = 
    | Assign of allsym list * expr * (line * column) (*a=10*)
    | VarDecl of typedSymbol * (line * column) (*create *)
    | CaseDef of case * (line * column)
    | Escape of string * (line * column)
    | Block of stmt list * (line * column)
    | Par of symbol * simpleExpr * stmt * (line * column)
    | For of symbol * simpleExpr * stmt * (line * column)
    | Noop
  and expr =
    | FCall of filterCall * bool
    | SimExpr of simpleExpr
  and case =
    | Case of caseClause list * otherwise * (line * column)
  and caseClause = 
    | Clause of relExpr * stmt * (line * column)
  and otherwise = 
    | Otherwise of stmt * (line * column)

  (* Top level  *)
  type filter = Filter of symbol * typedSymbol list * typedSymbol list * stmt

  type toplevelStmt = 
    | Def of filter * relExpr option * (line * column)
    | DefMain of filter * relExpr option * (line * column)
    | TopEscape of string * (line * column)

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
    | Node of stmt * filter * relExpr option * fcfg list
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
    | Topnode of stmt * string * relExpr option * cfg list (* These are my statements within a filter and its name *)
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


