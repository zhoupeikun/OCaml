%{

  open Ast

  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let mk_node e = { expr = e; pos = current_pos(); typ = Tunknown }

%}

%token AND
%token <Ast.binop> COMP
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token CONST_UNIT
%token DOUBLE_EQUAL
%token ELSE
%token EOF
%token EOI
%token EQUAL
%token <string> IDENT
%token IF
%token IN
%token LET
%token LPAREN
%token MINUS
%token NEQ
%token NOT
%token OR
%token PLUS
%token PRINT_INT
%token PRINT_NEWLINE
%token REC
%token RPAREN
%token SEMI
%token SLASH
%token STAR
%token THEN
/* DIFF */
%token BOOL
%token COLON
%token INT
%token UNIT
/* ENDIFF */

%nonassoc IN
%nonassoc ELSE
%left OR
%left AND
%left COMP EQUAL NEQ DOUBLE_EQUAL
%left PLUS MINUS
%left STAR SLASH
%nonassoc NOT

%start prog
%type <Ast.prog> prog

%%

prog:
| instr_seq EOF { $1 }
;

instr_seq:
| /* empty */    { [] }
| instr instr_seq       { $1 :: $2 }
;

instr:
| expr EOI
    { Icompute $1 }
/* DIFF */
| LET typed_ident EQUAL expr EOI
    { Ilet (fst $2, snd $2, $4) }
| LET IDENT typed_ident_list COLON typ EQUAL expr EOI
    { Ifun (false, $2, $3, $5, $7) }
| LET REC IDENT typed_ident_list COLON typ EQUAL expr EOI
    { Ifun (true, $3, $4, $6, $8) }
;

typ:
| UNIT
    { Tunit }
| BOOL
    { Tbool }
| INT
    { Tint }
;

typed_ident:
| IDENT COLON typ
    { ($1, $3) }

typed_ident_list:
| LPAREN typed_ident RPAREN
    { [$2] }
| LPAREN typed_ident RPAREN typed_ident_list
    { $2 :: $4 }
;
/* ENDIFF */

simple_expr:
| const
    { mk_node $1 }
| LPAREN expr RPAREN
    { $2 }
| IDENT
    { mk_node (Eident $1) }


expr:
| simple_expr
    { $1 }

| MINUS expr
    { mk_node (Eunop (Uminus, $2)) }
| expr PLUS expr
    { mk_node (Ebinop (Badd, $1, $3)) }
| expr MINUS expr
    { mk_node (Ebinop (Bsub, $1, $3)) }
| expr STAR expr
    { mk_node (Ebinop (Bmul, $1, $3)) }
| expr SLASH expr
    { mk_node (Ebinop (Bdiv, $1, $3)) }

| IF expr THEN expr ELSE expr
    { mk_node (Eif ($2, $4, $6)) }
| NOT expr
    { mk_node (Eunop (Unot, $2)) }
| expr DOUBLE_EQUAL expr
    { mk_node (Ebinop (Beq, $1, $3)) }
| expr NEQ expr
    { mk_node (Ebinop (Bneq, $1, $3)) }
| expr COMP expr
    { mk_node (Ebinop ($2, $1, $3)) }
| expr AND expr
    { mk_node (Ebinop (Band, $1, $3)) }
| expr OR expr
    { mk_node (Ebinop (Bor, $1, $3)) }

| LET IDENT EQUAL expr IN expr
    { mk_node (Eletin ($2, $4, $6)) }

| PRINT_INT simple_expr
    { mk_node (Eprint_int $2) }
| PRINT_NEWLINE simple_expr
    { mk_node (Eprint_newline $2) }

| IDENT args
    { mk_node (Eapp ($1, $2)) }

;


const:
| CONST_UNIT
    { Econst Cunit }
| CONST_BOOL
    { Econst (Cbool $1) }
| CONST_INT
    { Econst (Cint $1) }
;

args:
| simple_expr
    { [$1] }
| simple_expr args
    { $1 :: $2 }
;
