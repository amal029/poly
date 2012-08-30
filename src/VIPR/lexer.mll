{
  (* The header *)
  open Lexing
}

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | "//"_*'\n' {let () = new_line lexbuf in lexer lexbuf}
  | '\n' {let () = new_line lexbuf in lexer lexbuf}
  | '+'  {Parser.TPlus}
  | ':'  {Parser.TColon}
  | '-'  {Parser.TMinus}
  | '*'  {Parser.TTimes}
  | '/'  {Parser.TDiv}
  | '%' {Parser.TMod}
  | "\"" {Parser.Quote}
  | '^'  {Parser.TPow}
  | '('  {Parser.TOP}
  | ')'  {Parser.TCP}
  | '{'  {Parser.TOB}
  | '}'  {Parser.TCB}
  | '='  {Parser.TEqual}
  | "==" {Parser.TEqualEqual}
  | "<=" {Parser.TLessEqual}
  | ">=" {Parser.TGreaterEqual}
  | ','  {Parser.TComma}
  | '<'  {Parser.TLess}
  | '>'  {Parser.TGreater}
  | ">>"  {Parser.TRShift}
  | "<<"  {Parser.TLShift}
  | '['  {Parser.TLbrack}
  | ']'  {Parser.TRbrack}
  | "Par" {Parser.TPar}
  | "For" {Parser.TFor}
  | "If" {Parser.If}
  | "Program" {Parser.Program}
  | "Ground" {Parser.Ground}
  | "Aggregate" {Parser.Aggregate}
  | "Tile" {Parser.Tile}
  | "LitTrue" {Parser.LitTrue}
  | "LitFalse" {Parser.LitFalse}
  | "StaticIndex" {Parser.StaticIndex}
  | "DynamicIndex" {Parser.DynamicIndex}
  | "Array" {Parser.Array}
  | "Variable" {Parser.Variable}
  | "Subarray" {Parser.Subarray}
  | "StaticArrayRef" {Parser.StaticArrayRef}
  | "DynamicArrayRef" {Parser.DynamicArrayRef}
  | "VariableRef" {Parser.VariableRef}
  | "Constant" {Parser.Constant}
  | "Ref" {Parser.Ref}
  | "Binop" {Parser.Binop}
  | "Unop" {Parser.Unop}
  | "Brackets" {Parser.Brackets}
  | "Cast" {Parser.Cast}
  | "RBinop" {Parser.RBinop}
  | "RBrackets" {Parser.RBrackets}
  | "And" {Parser.And}
  | "Or" {Parser.Or}
  | "Not" {Parser.Not}
  | "Procedure" {Parser.Procedure}
  | "Declare" {Parser.Declare}
  | "DeclareFun" {Parser.DeclareFun}
  | "DeclareEntry" {Parser.DeclareEntry}
  | "Assign" {Parser.Assign}
  | "DeclareAndAssign" {Parser.DeclareAndAssign}
  | "DeclareArrayConst" {Parser.DeclareArrayConst}
  | "CallFun" {Parser.CallFun}
  | "Block" {Parser.Block}
  | "Noop" {Parser.Noop}
  | "Int" {Parser.TInt32}
  | "Int8" {Parser.TInt8}
  | "Int16" {Parser.TInt16}
  | "Int32" {Parser.TInt32}
  | "Int64" {Parser.TInt64}
  | "Int8s" {Parser.TInt8s}
  | "Int16s" {Parser.TInt16s}
  | "Int32s" {Parser.TInt32s}
  | "Int64s" {Parser.TInt64s}
  | "Float8" {Parser.TFloat8}
  | "Float16" {Parser.TFloat16}
  | "Float32" {Parser.TFloat32}
  | "Float" {Parser.TFloat32}
  | "Float64" {Parser.TFloat64}
  | "Boolean" {Parser.Boolean}
  | ['0'-'9']+ {Parser.TInt (lexeme lexbuf)} (* an integer *)
  | ['0'-'9']+'.'['0'-'9']+ {Parser.TFloat (lexeme lexbuf)} (* a floating number *)
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* {Parser.TSymbol (lexeme lexbuf)} (* any identifier a letter followed by anything, except a '$' sign*)
  | "/*" {comment 1 lexbuf} (* start of a comment *)
  | _  {lexer lexbuf} (* leave anything else *)
  | eof {Parser.TEof}
and comment depth = parse
  | "/*" {comment (depth + 1) lexbuf}
  | "*/" {if depth = 1 then lexer lexbuf else comment (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment depth lexbuf}
  | _ {comment depth lexbuf} 

{
(* The tail for now do nothing *)
}
