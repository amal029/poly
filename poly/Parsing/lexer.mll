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
  | "extern" {Parser.TExtern}
  | '-'  {Parser.TMinus}
  | '*'  {Parser.TTimes}
  | '/'  {Parser.TDiv}
  | '%' {Parser.TMod}
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
  | "in" {Parser.TIn}
  | "out" {Parser.TOut}
  | "split" {Parser.TSplit}
  | "otherwise" {Parser.TOtherwise}
  | "par" {Parser.TPar}
  | "for" {Parser.TFor}
  | "var" {Parser.TVar}
  | "&&" {Parser.And}
  | "||" {Parser.Or}
  | "where" {Parser.Where}
  | "main" {Parser.TMain}
  | "case" {Parser.TCase}
  | "int" {Parser.TInt32}
  | "int8" {Parser.TInt8}
  | "int16" {Parser.TInt16}
  | "int32" {Parser.TInt32}
  | "int64" {Parser.TInt64}
  | "int8s" {Parser.TInt8s}
  | "int16s" {Parser.TInt16s}
  | "int32s" {Parser.TInt32s}
  | "int64s" {Parser.TInt64s}
  | "float8" {Parser.TFloat8}
  | "float16" {Parser.TFloat16}
  | "float32" {Parser.TFloat32}
  | "float" {Parser.TFloat32}
  | "float64" {Parser.TFloat64}
  | ['0'-'9']+ {Parser.TInt (lexeme lexbuf)} (* an integer *)
  | "<!--"_*"-->"  {Parser.TEscapedCode (lexeme lexbuf)} (* start of escape code, does not work *)
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
(* The tail for now do nothing*)
}
