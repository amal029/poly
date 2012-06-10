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
%token TPlus TMinus TTimes TDiv TPow TOP TCP TEqual TOB TCB TComma TLess TLessEqual TGreater TGreaterEqual TEqualEqual
%token TLbrack TRbrack TColon TCase TEof TLShift TRShift TVar
%token TMain TIn TOut TOtherwise TPar TFor
%token TInt8 TInt16 TInt32 TInt64 TInt8s TInt16s TInt32s TInt64s TFloat8 TFloat32 TFloat64 TFloat16

/* Constructors with an argument */
%token <string> TInt
%token <string> TFloat
%token <string> TEscapedCode
%token <string> TSymbol

/* operator associative rules */
%left TPlus TMinus
%left TTimes TDiv 
%left TPow
%left TOP TCP
%nonassoc TUminus /* useful for difference between -2 and 1 - 2*/

/* The start of the parsing function */
%start ast
%type <Language.Language.ast> ast /* Test if this is correct */

%%
/* These are the parsing rules */

ast:
    | toplevelstmtlist TEof {Language.Language.Program($1)}
;

toplevelstmtlist:
    | toplevelstmtlist toplevelstmt {$2::$1}
    | toplevelstmt {[$1]}

toplevelstmt:
    | filter {Language.Language.Def ($1, ln())}
    | TMain filter {Language.Language.DefMain($2, ln())}
    | TEscapedCode {Language.Language.TopEscape ($1, ln())}
;

filter:
    | symbol TIn argumentlist_in TOut argumentlist_out stmt { Language.Language.Filter($1,$3,$5,$6)}
    | symbol stmt { Language.Language.Filter($1,[],[],$2)}
    | symbol TIn argumentlist_in stmt { Language.Language.Filter($1,$3,[],$4)}
    | symbol TOut argumentlist_out stmt { Language.Language.Filter($1,[],$3,$4)}
;

argumentlist_out:
    | argumentlist_out TComma argument_out {$1@[$3]}
    | argument_out {[$1]}
;

argumentlist_in:
    | argumentlist_in TComma argument_input {$1@[$3]}
    | argument_input {[$1]}
;

argument_out:
    | addrSymbol {Language.Language.ComTypedSymbol(Language.DataTypes.None,$1,ln())}
    | symbol {Language.Language.SimTypedSymbol(Language.DataTypes.None,$1,ln())}
    | typedsymbol {$1}
;

argument_input:
    | addrSymbol { let () = counter := !counter +1 in Language.Language.ComTypedSymbol(Language.DataTypes.Poly ("'a" ^ (string_of_int (!counter))),$1, ln())}
    | symbol {let () = counter := !counter +1 in Language.Language.SimTypedSymbol(Language.DataTypes.Poly ("'a" ^ (string_of_int (!counter))),$1, ln())}
    | typedsymbol {$1}
;

stmtlist:
    | stmtlist stmt {$2::$1}
    | stmt {[$1]}
;

stmt:
    | allsymlist TEqual expr {Language.Language.Assign($1,$3, ln())}
    | TOP allsymlist TCP TEqual expr {Language.Language.Assign($2,$5, ln())} /* this is just a tuple back */
    | typedsymbol {Language.Language.VarDecl($1, ln())}
    | varsymbol {Language.Language.VarDecl($1, ln())}
    | TEscapedCode  {Language.Language.Escape ($1, ln())}
    | TOB stmtlist TCB {Language.Language.Block ($2, ln())}
    | TOB TCB {Language.Language.Noop}
    | case {Language.Language.CaseDef ($1, ln())}
    | iter {$1}
;

iter:
    | TPar symbol TIn colonExpr stmt {Language.Language.Par($2,$4,$5, ln())}
    | TPar stmt {Language.Language.Par(Language.Language.Symbol("NULL", ln()),
				       Language.Language.ColonExpr(Language.Language.Const (Language.DataTypes.Int32, "$i", ln()),
								   Language.Language.Const(Language.DataTypes.Int32,"1", ln()),
								   Language.Language.Const(Language.DataTypes.Int32,"'size", ln()), ln()),$2, ln())}
    | TFor symbol TIn colonExpr stmt {Language.Language.For($2,$4,$5, ln())}

case:
    | TCase caseclauselist otherwise {Language.Language.Case($2,$3)}
;
caseclauselist:
    | caseclauselist caseclause {$2::$1}
    | caseclause {[$1]}
;
caseclause:
    | TOP relExpr TCP stmt {Language.Language.Clause ($2,$4)}
;
otherwise:
    | TOtherwise stmt {Language.Language.Otherwise ($2)}
;

allsymlist:
    | allsym {[$1]}
    | allsymlist TComma allsym {$1@[$3]}
;

allsym:
    | addrSymbol {Language.Language.AllAddressedSymbol ($1)}
    | symbol {Language.Language.AllSymbol($1)}
    | typedsymbol {Language.Language.AllTypedSymbol ($1)}
    | varsymbol {Language.Language.AllTypedSymbol ($1)}
;

expr:
    | fcall {Language.Language.FCall ($1)}
    | simpleExpr {Language.Language.SimExpr ($1)}
;

fcall:
    | symbol TOP callargumentlist TCP {Language.Language.Call ($1,$3, ln())}
    | symbol TOP TCP {Language.Language.Call ($1,[], ln())}
;

callargumentlist:
    | callargumentlist TComma callargument {$1@[$3]}
    | callargument {[$1]}
;

callargument:
    | addrSymbol {Language.Language.CallAddrressedArgument($1)}
    | symbol {Language.Language.CallSymbolArgument ($1)}
;

relExpr:
    | simpleExpr TLess simpleExpr {Language.Language.LessThan($1,$3, ln())}
    | simpleExpr TLessEqual simpleExpr {Language.Language.LessThanEqual($1,$3, ln())}
    | simpleExpr TGreater simpleExpr {Language.Language.GreaterThan($1,$3, ln())}
    | simpleExpr TGreaterEqual simpleExpr {Language.Language.GreaterThanEqual($1,$3, ln())}
    | simpleExpr TEqualEqual simpleExpr {Language.Language.EqualTo($1,$3, ln())}
;

angledimlist:
    | angledimlist angledim {$1@[$2]}
    /*| angledim {[$1]}*/

angledim:
    | TLess dimSpecExpr TGreater {Language.Language.AngleDimExpr($2)}
;

dimspeclist:
    | dimspeclist dimspec {$1@[$2]}
    | dimspec {[$1]}
;

/*[2][3:4:5]....*/
dimspec:
    | TLbrack dimspecExprlist TRbrack {Language.Language.BracDim ($2)}
;

dimspecExprlist:
    | dimspecExprlist TComma dimSpecExpr {$1@[$3]}
    | dimSpecExpr {[$1]}
;

/*2, 2:4:2, 2:4:2 4:5:4, TStar */
dimSpecExpr:
    | pdimSpec {Language.Language.DimSpecExpr($1)}
    | TTimes {Language.Language.DimSpecExpr(Language.Language.TStar)}
    /*| TTimes TTimes {Language.Language.DimSpecExpr(Language.Language.TStarStar)}*/
;

/*2 (a+2),....,etc*/
pdimSpec:
    | simpleExpr {$1}
    /*| colonExpr {$1}*/
;


colonExpr:
    | simpleExpr TColon  simpleExpr TColon simpleExpr {Language.Language.ColonExpr($1,$3,$5, ln())}
    | simpleExpr TColon  simpleExpr {Language.Language.ColonExpr($1,$3, Language.Language.Const(Language.DataTypes.Int32, "1", ln()), ln())}
;

simpleExpr:
    | simpleExpr TPlus simpleExpr {Language.Language.Plus ($1, $3, ln())}
    | simpleExpr TMinus simpleExpr {Language.Language.Minus ($1, $3, ln())}
    | simpleExpr TTimes simpleExpr {Language.Language.Times ($1, $3, ln())}
    | simpleExpr TPow simpleExpr {Language.Language.Pow ($1, $3, ln())}
    | simpleExpr TDiv simpleExpr {Language.Language.Div ($1, $3, ln())}
    | TOP simpleExpr TCP {Language.Language.Brackets ($2, ln())}
    | addrSymbol {Language.Language.AddrRef($1,ln())}
    | symbol {Language.Language.VarRef ($1, ln())}
    | TInt {Language.Language.Const (Language.DataTypes.Int32s,$1, ln())} /*e.g: 8, the type should be found using type inference, what now??*/
    | TFloat {Language.Language.Const (Language.DataTypes.Float32, $1, ln())} /*e.g.: 8.0, this should be done using type inference, what now??*/
    | TOP dataTypes TCP simpleExpr {Language.Language.Cast ($2,$4,ln())}
    | TMinus simpleExpr %prec TUminus {Language.Language.Opposite($2,ln())}
;

varsymbol:
    | TVar symbol {Language.Language.SimTypedSymbol (Language.DataTypes.None, $2, ln())}
    | TVar addrSymbol {Language.Language.ComTypedSymbol (Language.DataTypes.None, $2, ln())}
;

typedsymbol:
    | dataTypes symbol {Language.Language.SimTypedSymbol ($1, $2, ln())} /*e.g.: int8s t*/
    | dataTypes addrSymbol {Language.Language.ComTypedSymbol ($1,$2,ln())}
;
symbol:
    | TSymbol {Language.Language.Symbol ($1, ln())} /*e.g.: t*/
;
addrSymbol:
    | symbol dimspeclist {
      let ll = List.flatten (List.map (fun x -> match x with Language.Language.BracDim x -> x) $2) in
      Language.Language.AddressedSymbol($1,[], [Language.Language.BracDim ll], ln())} /* a<><>[][]... */
    | symbol angledimlist {Language.Language.AddressedSymbol($1,$2,[],ln())} /* a<><>[][]... */
    | symbol angledimlist dimspeclist {
      let ll = List.flatten (List.map (fun x -> match x with Language.Language.BracDim x -> x) $3) in
      Language.Language.AddressedSymbol($1,$2,[Language.Language.BracDim ll],ln())} /* a<><>[][]... */
;
/* Types */
dataTypes:
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
;
%%
(* This is the trailer *)
