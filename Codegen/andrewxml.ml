(* 
   
   Author: Avinash Malik

   Purpose: Exists to produce the xml that
   andrew's compiler can take in to produce 'C' using haskell 

   Date: Wed May 30 19:01:03 IST 2012
*)

open Xml
open AndrewLang
open AndrewLang

exception Internal_compiler_error of string

let symbol_xml = function
  | Symbol x -> Element ("Symbol",[],[PCData x])

let value_xml x = Element ("Value",[],[PCData x])

let rec simexpr_xml = function
  | Plus (x,y) -> Element("Plus",[],[(simexpr_xml x);(simexpr_xml y)])
  | Div (x,y) -> Element("Div",[],[(simexpr_xml x);(simexpr_xml y)])
  | Mod (x,y) -> Element("Mod",[],[(simexpr_xml x);(simexpr_xml y)])
  | Times (x,y) -> Element("Times",[],[(simexpr_xml x);(simexpr_xml y)])
  | Pow (x,y)-> Element("Pow",[],[(simexpr_xml x);(simexpr_xml y)])
  | Minus (x,y) -> Element("Minus",[],[(simexpr_xml x);(simexpr_xml y)])
  | Const (x,y) -> Element("Const",[],[(DataTypes.datatype_xml x);(value_xml y)])
  | Ref x -> Element("Ref",[],[(rsymbol_xml x)])
  | Brackets x -> Element("Brackets",[],[(simexpr_xml x)])
  | Negate x -> Element("Negate",[],[(simexpr_xml x)])
  | Cast (x,y) -> Element("Cast",[],[(DataTypes.datatype_xml x);(simexpr_xml y)])
  | ColonExpr x -> dimspec_xml x

and typedsymbol_xml = function
  | TypedSymbol (x,y) -> 
    let dtype = DataTypes.datatype_xml x in
    let symbol = symbol_xml y in
    Element ("TypedSymbol",[],[dtype;symbol])
  | TypedAddressedSymbol (x,y) -> Element("TypedAddressedSymbol",[],[(DataTypes.datatype_xml x);(addressedSymbol_xml y)])

(* and addresstypedsymbol = function *)

and dimspec_xml = function
  | VarDimSpec (x,y,z) -> 
    let xsim = Element("Start",[],[simexpr_xml x]) in
    let zsim = Element("Stride",[],[(match z with 
      | None -> PCData("1") 
      | Some x -> (simexpr_xml x))]) in
    let ysim = Element("End",[],[(match y with | None -> (simexpr_xml x) | Some x -> (simexpr_xml x))]) in
    Element("VarDimSpec",[],[xsim;ysim;zsim])

and comprehension_xml = function
  | Comprehension x -> Element("Comprehension",[], (List.map (fun x -> dimspec_xml x) x))

and addressedSymbol_xml = function
  | AddressedSymbol (x,y) ->
    let symbol = symbol_xml x in
    let comprehension = comprehension_xml y in
    Element("AddressedSymbol",[],[symbol;comprehension])

and rsymbol_xml = function
  | RSym x -> Element("RSym",[],[symbol_xml x])
  | RASym x -> Element("RASym",[],[addressedSymbol_xml x])

let relExpr_xml = function
  | LessThanEqual (x,y) -> Element("LessThanEqual",[],[(simexpr_xml x);(simexpr_xml y)])
  | LessThan (x,y) ->Element("LessThan",[],[(simexpr_xml x);(simexpr_xml y)])
  | GreaterThanEqual (x,y) -> Element("GreaterThanEqual",[],[(simexpr_xml x);(simexpr_xml y)])
  | GreaterThan (x,y) -> Element("GreaterThan",[],[(simexpr_xml x);(simexpr_xml y)])
  | EqualTo (x,y) -> Element("EqualTo",[],[(simexpr_xml x);(simexpr_xml y)])

let callinputlist_xml x = Element ("CallInputList",[],(List.map (fun x -> rsymbol_xml x) x))
let calloutputlist_xml x = Element ("CallOutputList",[],(List.map (fun x -> rsymbol_xml x) x))

let functioncall_xml = function
  | Call (x,y,z) -> 
    let sym = symbol_xml x in
    let outs = (calloutputlist_xml z) in
    let ins = (callinputlist_xml z) in
    Element("Call",[],([sym;outs;ins]))

let extern_xml = function
  | true -> ("extern","true")
  | false -> ("extern","false")

let rec stmt_xml = function
  | Escape x -> Element ("Escape",[],[PCData x])
  | CaseDef x -> Element("CaseDef",[],[(case_xml x)])
  | FCall (x,e) -> Element("FCall",[extern_xml e],[(functioncall_xml x)])
  | DeclAssign (x,y) -> Element("DeclAssign",[],[(typedsymbol_xml x);(simexpr_xml y)])
  | AggregateDeclAssign (x,y) -> Element("DeclAssignAggregateConst",[],(List.map (fun x -> value_xml x) y))
  | VarDecl x ->  Element("VarDecl",[],[(typedsymbol_xml x)])
  | Assign (x,y) -> Element("Assign",[],[(rsymbol_xml x);(simexpr_xml y)])
  | For (x,y,z) -> Element("For",[],[(symbol_xml x);(dimspec_xml y);(block_xml z)])
  | Par (x,y,z) -> Element("Par",[],[(symbol_xml x);(dimspec_xml y);(block_xml z)])

and block_xml x = Element("StmtList",[],(List.map (fun x -> stmt_xml x) x))

and case_xml = function
  | Case (x,y) -> 
    let clause_list = List.map (fun x -> caseClause_xml x) x in
    let o = otherwise_xml y in
    Element("Case",[],(o::clause_list))

and caseClause_xml = function
  | Clause (x,y) -> Element("Clause",[],[(relExpr_xml x);(block_xml y)])

and otherwise_xml = function
  | Otherwise x -> Element("Otherwise",[],[(block_xml x)])


let inputlist_xml x = Element ("InputList", [], (List.map(fun x -> typedsymbol_xml x) x))
let outputlist_xml x = Element ("OutputList",[], (List.map(fun x -> typedsymbol_xml x) x))

let filter_xml = function
  | Function (x,y,z,b) -> 
    let symbol = symbol_xml x in
    let inputlist = inputlist_xml y in
    let outputlist = outputlist_xml z in
    let block = block_xml b in
    Element ("Function", [], [symbol;inputlist;outputlist;block])

let toplevelstmt_xml = function
  | Def x -> Element("Def", [], [filter_xml x])
  | DefMain x -> Element ("DefMain", [], [filter_xml x]) 
  | TopEscape x -> Element("TopEscape",[],[PCData x])

let ast_xml file = function
  | Program x -> 
    let toplevelStmtList = List.map (fun x -> toplevelstmt_xml x) x in
    let ret = Element ("Program", [], [Element ("ToplevelStmtList",[],toplevelStmtList)]) in
    (* Write the output file to file *)
    let sret = to_string_fmt ret in
    let ochan = open_out file in
    output_string ochan sret;
    flush ochan; (*flush everything down*)
    close_out ochan
