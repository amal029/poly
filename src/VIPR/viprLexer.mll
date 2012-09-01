{
  (* The header *)
  open Lexing
}

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | "//"_*'\n' {let () = new_line lexbuf in lexer lexbuf}
  | '\n' {let () = new_line lexbuf in lexer lexbuf}
  | '+'  {ViprParser.TPlus}
  | ':'  {ViprParser.TColon}
  | '-'  {ViprParser.TMinus}
  | '*'  {ViprParser.TTimes}
  | '/'  {ViprParser.TDiv}
  | '%' {ViprParser.TMod}
  | "\"" {ViprParser.Quote}
  | '^'  {ViprParser.TPow}
  | '('  {ViprParser.TOP}
  | ')'  {ViprParser.TCP}
  | '{'  {ViprParser.TOB}
  | '}'  {ViprParser.TCB}
  | '='  {ViprParser.TEqual}
  | "==" {ViprParser.TEqualEqual}
  | "<=" {ViprParser.TLessEqual}
  | ">=" {ViprParser.TGreaterEqual}
  | ','  {ViprParser.TComma}
  | '<'  {ViprParser.TLess}
  | '>'  {ViprParser.TGreater}
  | ">>"  {ViprParser.TRShift}
  | "<<"  {ViprParser.TLShift}
  | '['  {ViprParser.TLbrack}
  | ']'  {ViprParser.TRbrack}
  | "Par" {ViprParser.TPar}
  | "For" {ViprParser.TFor}
  | "If" {ViprParser.If}
  | "Program" {ViprParser.Program}
  | "Ground" {ViprParser.Ground}
  | "Aggregate" {ViprParser.Aggregate}
  | "Tile" {ViprParser.Tile}
  | "LitTrue" {ViprParser.LitTrue}
  | "LitFalse" {ViprParser.LitFalse}
  | "StaticIndex" {ViprParser.StaticIndex}
  | "DynamicIndex" {ViprParser.DynamicIndex}
  | "Array" {ViprParser.Array}
  | "Variable" {ViprParser.Variable}
  | "Subarray" {ViprParser.Subarray}
  | "StaticArrayRef" {ViprParser.StaticArrayRef}
  | "DynamicArrayRef" {ViprParser.DynamicArrayRef}
  | "VariableRef" {ViprParser.VariableRef}
  | "Constant" {ViprParser.Constant}
  | "Ref" {ViprParser.Ref}
  | "Binop" {ViprParser.Binop}
  | "Unop" {ViprParser.Unop}
  | "Brackets" {ViprParser.Brackets}
  | "Cast" {ViprParser.Cast}
  | "RBinop" {ViprParser.RBinop}
  | "RBrackets" {ViprParser.RBrackets}
  | "And" {ViprParser.And}
  | "Or" {ViprParser.Or}
  | "Not" {ViprParser.Not}
  | "Procedure" {ViprParser.Procedure}
  | "Declare" {ViprParser.Declare}
  | "DeclareFun" {ViprParser.DeclareFun}
  | "DeclareEntry" {ViprParser.DeclareEntry}
  | "Assign" {ViprParser.Assign}
  | "DeclareAndAssign" {ViprParser.DeclareAndAssign}
  | "DeclareArrayConst" {ViprParser.DeclareArrayConst}
  | "CallFun" {ViprParser.CallFun}
  | "Block" {ViprParser.Block}
  | "Noop" {ViprParser.Noop}
  | "Int" {ViprParser.TInt32}
  | "Int8" {ViprParser.TInt8}
  | "Int16" {ViprParser.TInt16}
  | "Int32" {ViprParser.TInt32}
  | "Int64" {ViprParser.TInt64}
  | "Int8s" {ViprParser.TInt8s}
  | "Int16s" {ViprParser.TInt16s}
  | "Int32s" {ViprParser.TInt32s}
  | "Int64s" {ViprParser.TInt64s}
  | "Float8" {ViprParser.TFloat8}
  | "Float16" {ViprParser.TFloat16}
  | "Float32" {ViprParser.TFloat32}
  | "Float" {ViprParser.TFloat32}
  | "Float64" {ViprParser.TFloat64}
  | "Boolean" {ViprParser.Boolean}
  | ['0'-'9']+ {ViprParser.TInt (lexeme lexbuf)} (* an integer *)
  | ['0'-'9']+'.'['0'-'9']+ {ViprParser.TFloat (lexeme lexbuf)} (* a floating number *)
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* {ViprParser.TSymbol (lexeme lexbuf)} (* any identifier a letter followed by anything, except a '$' sign*)
  | "/*" {comment 1 lexbuf} (* start of a comment *)
  | _  {lexer lexbuf} (* leave anything else *)
  | eof {ViprParser.TEof}
and comment depth = parse
  | "/*" {comment (depth + 1) lexbuf}
  | "*/" {if depth = 1 then lexer lexbuf else comment (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment depth lexbuf}
  | _ {comment depth lexbuf} 

{
(* The tail for now do nothing *)
}
