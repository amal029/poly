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
%token And Or Where
%token TLbrack TRbrack TColon TCase TEof TLShift TRShift TVar
%token TMain TIn TOut TOtherwise TPar TFor
%token TInt8 TInt16 TInt32 TInt64 TInt8s TInt16s TInt32s TInt64s TFloat8 TFloat32 TFloat64 TFloat16
%token TExtern TSplit

/* Constructors with an argument */
%token <string> TInt
%token <string> TFloat
%token <string> TEscapedCode
%token <string> TSymbol

/* operator associative rules */
(* %left TRShift TLShift *)
(* %left TPlus TMinus *)
(* %left TTimes TDiv TMod *)
(* %left TPow *)
(* %left TOP TCP *)
(* %nonassoc TUminus /* useful for difference between -2 and 1 - 2*/ *)

/* The start of the parsing function */
%start ast
%type <VIPR.ast> ast /* Test if this is correct */

%%
/* These are the parsing rules */

ast:
    | Program TLbrack statementlist TRbrack TEof {VIPR.Program($3)}
;

statementlist:
    | statementlist TComma statement {$1@[$3]}
    | statement {[$1]}
;
statement : 
    | TOP If rExpression statement statement TCP                              { VIPR.If ($3,$4,$5) }
    | TOP For storage expression rExpression statement statement TCP          { VIPR.For ($3,$4,$5,$6,$7) }
    | TOP Par storage expression rExpression statement statement TCP          { VIPR.Par($3,$4,$5,$6,$7) }
    | TOP Declare storage TCP                                                 { VIPR.Declare $3 }
    | TOP DeclareFun proc TCP                                                 { VIPR.DeclareFun $3 }
    | TOP DeclareEntry proc TCP                                               { VIPR.DeclareEntry $3 }
    | TOP Assign reference expression TCP                                     { VIPR.Assign ($3,$4) }
    | TOP DeclareAndAssign storage expression TCP                             { VIPR.DeclareAndAssign ($3,$4) }
    | TOP DeclareArrayConst storage TLbrack numList TRbrack TCP                       { VIPR.DeclareArrayConst ($3, $5) }
    | TOP CallFun literal TLbrack referenceList TRbrack TLbrack referenceList TRbrack TCP     { VIPR.CallFun ($3 ,$5, $8) }
    | TOP Block TLbrack statementList TRbrack TCP                                     { VIPR.Block $4 }
    | TOP Noop TCP                                                            { VIPR.Noop }
;

proc : 
    | TOP Procedure literal TLbrack StorageList TRbrack TLbrack StorageList TRbrack Statement TCP  { VIPR.Procedure ($3,$5,$8,$10) }
;

storageList : 
    | storageList TComma storage                                       { $1@[$3] }
    | storage                                                       { [$1] }
;


rExpression : 
    | LitTrue                                                          { VIPR.LitTrue }
    | LitFalse                                                         { VIPR.LitFalse }
    | TOP RBinop op expression expression TCP                       { VIPR.RBinop ($3,$4,$5) }
    | TOP RBrackets rExpression TCP                                 { VIPR.RBrackets $3 }
    | TOP And rExpression rExpression TCP                           { VIPR.And ($3,$4) }
    | TOP Or rExpression rExpression TCP                            { VIPR.Or ($3,$4) }
    | TOP Not rExpression TCP                                       { VIPR.Not $3 }
;

expression : 
    | TOP Ref reference TCP                                          { VIPR.Ref $3 }
    | TOP Binop op expression expression TCP                         { VIPR.Binop ($3,$4,$5) }
    | TOP Unop op expression TCP                                     { VIPR.Unop ($3,$4) }
    | TOP Brackets expression TCP                                    { VIPR.Brackets $3 }
    | TOP Cast groundType expression TCP                             { VIPR.Cast ($3,$4) }
;

reference : 
    | TOP StaticArrayRef literal index TCP                            { VIPR.StaticArrayRef ($3,$4) }
    | TOP DynamicArrayRef literal index TCP                           { VIPR.DynamicArrayRef ($3,$4) }
    | TOP VariableRef literal TCP                                     { VIPR.VariableRef $3 }
    | TOP Constant num TCP                                            { VIPR.Constant $3 }
;

storage : 
    | TOP Array literal typ TCP                                        { VIPR.Array ($3,$4) }
    | TOP Variable literal groundType TCP                               { VIPR.Variable ($3,$4) }
    | TOP Subarray literal typ typ index TCP                          { VIPR.Subarray ($3,$4,$5,$6) }
;

index : 
    | TOP StaticIndex TLbrack iList TRbrack TCP                                   { VIPR.StaticIndex $4 }
    | TOP DynamicIndex TLbrack expressionList TRbrack TCP                         { VIPR.DynamicIndex $4 }
;

literal : 
    | TSymbol                                                             { $1 }
;

intList : 
    | intList TComma int                                                   { $1@[$3] }
    | int                                                               { [$1] }
;

expressionList : 
    | expressionList TComma expression                              { $1 @ [$3] }
    | expression                                                 { [$1] }
;



op:
    | TPow {VIPR.POW}
    | TPlus {VIPR.PLUS}
    | TMinus {VIPR.MINUS}
    | TDiv {VIPR.DIV}
    | TTimes {VIPR.TIMES}
    | TMod {VIPR.MOD}
    | TEqualEqual {VIPR.EQEQ}
    | TEqual {VIPR.EQ}
    | TLessEqual {VIPR.LEQ}
    | TLess {VIPR.LT}
    | TGreaterEqual {VIPR.GEQ}
    | TGreater {VIPR.GT}
    | TRShift {VIPR.RSHIFT}
    | TLShift {VIPR.LSHIFT}

typ : 
    | TOP Ground groundType TCP                                            { VIPR.Ground $3 }
    | TOP Aggregate TLbrack iList TRbrack groundType TCP                           { VIPR.Aggregate ($4, $6) }
    | TOP Tile TLbrack iList TRbrack TLbrack iList TRbrack groundType TCP                  { VIPR.Tile ($4,$7,$9) }
;

iList : 
    | iList TComma int   { $1 @ [$3] }
    | int             { [$1] }
;

groundType:
    | TInt8 {VIPR.DataTypes.Int8}
    | TInt16 {VIPR.DataTypes.Int16}
    | TInt32 {VIPR.DataTypes.Int32}
    | TInt64 {VIPR.DataTypes.Int64}
    | TInt8s {VIPR.DataTypes.Int8s}
    | TInt16s {VIPR.DataTypes.Int16s}
    | TInt32s {VIPR.DataTypes.Int32s}
    | TInt64s {VIPR.DataTypes.Int64s}
    | TFloat8 {VIPR.DataTypes.Float8}
    | TFloat16 {VIPR.DataTypes.Float16}
    | TFloat32 {VIPR.DataTypes.Float32}
    | TFloat64 {VIPR.DataTypes.Float64}
;

numList : 
    | numList TComma num                                                { $1@[$3] }
    | num                                                               { [$1] }
;

num : 
    | Quote TInt Quote { $2 }
    | Quote TFloat Quote { $2 }
;

int:
    | TInt {$1}
;

%%
(* This is the trailer *)
