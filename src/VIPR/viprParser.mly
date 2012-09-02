%{
(* This is the header *)
 (* let _ = Parsing.set_trace true in () *)
 let counter = ref 0
 let line_nums = Hashtbl.create (1000) 
 let parse_error s = 
   let mypos = Parsing.symbol_start_pos() in
   print_string (s ^ " (line number: ");
   print_int mypos.Lexing.pos_lnum;
   print_string (", column number: " ^ (string_of_int (mypos.Lexing.pos_cnum - mypos.Lexing.pos_bol)));
   print_endline ")";
   flush stdout
 let ln () = 
   let mypos = Parsing.symbol_start_pos() in 
   let cnum = (mypos.Lexing.pos_cnum) - (mypos.Lexing.pos_bol) in
   (mypos.Lexing.pos_lnum,cnum)
%}

/* These are the declarations */

/* The tokens */
/* Constant constructors */
%token TPlus TMinus TTimes TDiv TPow TOP TCP TEqual TOB TCB TComma TLess TLessEqual TGreater TGreaterEqual TEqualEqual TMod
%token And Or Where Quote If Program Ground Aggregate Tile LitTrue LitFalse StaticIndex DynamicIndex Array Variable Subarray
%token TLbrack TRbrack TColon TCase TEof TLShift TRShift TVar StaticArrayRef DynamicArrayRef VariableRef Constant Ref
%token TMain TIn TOut TOtherwise TPar TFor Binop Unop Brackets Cast RBinop RBrackets And Or Not Procedure Declare
%token TInt8 TInt16 TInt32 TInt64 TInt8s TInt16s TInt32s TInt64s TFloat8 TFloat32 TFloat64 TFloat16 DeclareFun Assign
%token TExtern TSplit DeclareEntry DeclareAndAssign DeclareArrayConst CallFun Block Noop Boolean None

/* Constructors with an argument */
%token <string> TInt
%token <string> TFloat
%token <string> TEscapedCode
%token <string> TSymbol

/* The start of the parsing function */
%start ast
%type <Vipr.ast> ast /* Test if this is correct */

%%
/* These are the parsing rules */

ast:
    | Program TLbrack statementlist TRbrack TEof {Vipr.Program($3)}
;

statementlist:
    | statementlist TComma statement {$1@[$3]}
    | statement {[$1]}
;
statement : 
    | TOP If rExpression statement statement TCP                              { Vipr.If ($3,$4,$5) }
    | TFor storage expression rExpression statement statement { Vipr.For ($2,$3,$4,$5,$6) }
    | TPar storage expression rExpression statement statement { Vipr.Par($2,$3,$4,$5,$6) }
    | TOP Declare storage TCP                                                 { Vipr.Declare $3 }
    | DeclareFun proc { Vipr.DeclareFun $2 }
    | DeclareEntry proc { Vipr.DeclareEntry $2 }
    | TOP Assign reference expression TCP                                     { Vipr.Assign ($3,$4) }
    | TOP DeclareAndAssign storage expression TCP                             { Vipr.DeclareAndAssign ($3,$4) }
    | TOP DeclareArrayConst storage TLbrack numList TRbrack TCP                       { Vipr.DeclareArrayConst ($3, $5) }
    | TOP CallFun literal TLbrack referenceList TRbrack TLbrack referenceList TRbrack TCP     { Vipr.CallFun ($3 ,$5, $8) }
    | TOP Block TLbrack statementList TRbrack TCP                                     { Vipr.Block $4 }
    | TOP Noop TCP                                                            { Vipr.Noop }
;

statementList:
    | statementList TComma statement {$1 @ [$3]}
    | statement {[$1]}
;

referenceList:
    | referenceList TComma reference {$1 @ [$3]}
    | reference {[$1]}
;

proc : 
    | TOP Procedure literal TLbrack storageList TRbrack TLbrack storageList TRbrack statement TCP  { Vipr.Procedure ($3,$5,$8,$10) }
;

storageList : 
    | storageList TComma storage                                       { $1@[$3] }
    | storage                                                       { [$1] }
;


rExpression : 
    | LitTrue                                                          { Vipr.LitTrue }
    | LitFalse                                                         { Vipr.LitFalse }
    | TOP RBinop op expression expression TCP                       { Vipr.RBinop ($3,$4,$5) }
    | TOP RBrackets rExpression TCP                                 { Vipr.RBrackets $3 }
    | TOP And rExpression rExpression TCP                           { Vipr.And ($3,$4) }
    | TOP Or rExpression rExpression TCP                            { Vipr.Or ($3,$4) }
    | TOP Not rExpression TCP                                       { Vipr.Not $3 }
;

expression : 
    | TOP Ref reference TCP                                          { Vipr.Ref $3 }
    | TOP Binop op expression expression TCP                         { Vipr.Binop ($3,$4,$5) }
    | TOP Unop op expression TCP                                     { Vipr.Unop ($3,$4) }
    | TOP Brackets expression TCP                                    { Vipr.Brackets $3 }
    | TOP Cast groundType expression TCP                             { Vipr.Cast ($3,$4) }
;

reference : 
    | TOP StaticArrayRef literal index TCP                            { Vipr.StaticArrayRef ($3,$4) }
    | TOP DynamicArrayRef literal index TCP                           { Vipr.DynamicArrayRef ($3,$4) }
    | TOP VariableRef literal TCP { Vipr.VariableRef $3 }
    | TOP Constant num TCP                                            { Vipr.Constant $3 }
;

storage : 
    | Array literal typ { Vipr.Array ($2,$3) }
    | TOP Variable literal groundType TCP { Vipr.Variable ($3,$4) }
    | TOP Subarray literal typ typ index TCP                          { Vipr.Subarray ($3,$4,$5,$6) }
;

index : 
    | TOP StaticIndex TLbrack iList TRbrack TCP                                   { Vipr.StaticIndex $4 }
    | TOP DynamicIndex TLbrack expressionList TRbrack TCP                         { Vipr.DynamicIndex $4 }
;

literal : 
    | Quote TSymbol Quote                                                         { $2 }
;

expressionList : 
    | expressionList TComma expression                              { $1 @ [$3] }
    | expression                                                 { [$1] }
;

op:
    | Quote TPow Quote {Vipr.POW}
    | Quote TPlus Quote {Vipr.PLUS}
    | Quote TMinus Quote {Vipr.MINUS}
    | Quote TDiv Quote {Vipr.DIV}
    | Quote TTimes Quote {Vipr.TIMES}
    | Quote TMod Quote {Vipr.MOD}
    | Quote TEqualEqual Quote {Vipr.EQEQ}
    | Quote TEqual Quote {Vipr.EQ}
    | Quote TLessEqual Quote {Vipr.LEQ}
    | Quote TLess Quote {Vipr.LT}
    | Quote TGreaterEqual Quote {Vipr.GEQ}
    | Quote TGreater Quote {Vipr.GT}
    | Quote TRShift Quote {Vipr.RSHIFT}
    | Quote TLShift Quote {Vipr.LSHIFT}
;

typ : 
    | TOP Ground groundType TCP                                            { Vipr.Ground $3 }
    | TOP Aggregate TLbrack iList TRbrack groundType TCP                           { Vipr.Aggregate ($4, $6) }
    | TOP Tile TLbrack iList TRbrack TLbrack iList TRbrack groundType TCP                  { Vipr.Tile ($4,$9,$7) }
;

iList : 
    | iList TComma TInt   { $1 @ [int_of_string $3] }
    | TInt             { [int_of_string $1] }
;

groundType:
    | TInt8 {Language.DataTypes.Int8}
    | TInt16 {Language.DataTypes.Int16}
    | TInt32 {Language.DataTypes.Int32}
    | TInt64 {Language.DataTypes.Int64}
    | TInt8s {Language.DataTypes.Int8s}
    | TInt16s {Language.DataTypes.Int16s}
    | TInt32s {Language.DataTypes.Int32s}
    | TInt64s {Language.DataTypes.Int64s}
    | TFloat8 {Language.DataTypes.Float8}
    | TFloat16 {Language.DataTypes.Float16}
    | TFloat32 {Language.DataTypes.Float32}
    | TFloat64 {Language.DataTypes.Float64}
    | Boolean {Language.DataTypes.Bool}
    | None {Language.DataTypes.None}
;

numList : 
    | numList TComma num                                                { $1@[$3] }
    | num                                                               { [$1] }
;

num : 
    | Quote TInt Quote { $2 }
    | Quote TFloat Quote { $2 }
;

%%
(* This is the trailer *)
