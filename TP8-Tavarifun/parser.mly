%{

  open Ast

  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let mk_node e = { expr = e; pos = current_pos(); typ = Tunknown }

%}

%token AND
%token BOOL
%token COLON
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
%token INT
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
%token UNIT
/* DIFF - Références */
%token REF
%token GET_REF
%token SET_REF
/* ENDIFF */
/* DIFF - Options */
%token NONE
%token SOME
%token OPTION
/* ENDIFF */
/* DIFF - Structures */
%token TYPE
%token MUTABLE
%token LBRACE
%token RBRACE
%token SEMI
%token DOT
%token LT_MINUS
/* ENDIFF */

%nonassoc IN
%nonassoc ELSE
%nonassoc LT_MINUS
%nonassoc SET_REF
%left OR
%left AND
%left COMP EQUAL NEQ DOUBLE_EQUAL
%left PLUS MINUS
%left STAR SLASH
%nonassoc SOME
%left GET_REF
%left DOT
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
| LET typed_ident EQUAL expr EOI
    { Ilet (fst $2, snd $2, $4) }
| LET IDENT typed_ident_list COLON typ EQUAL expr EOI
    { Ifun (false, $2, $3, $5, $7) }
| LET REC IDENT typed_ident_list COLON typ EQUAL expr EOI
    { Ifun (true, $3, $4, $6, $8) }
/* DIFF - Structures */
| TYPE IDENT EQUAL LBRACE field_decl_seq RBRACE EOI
    { Istruct ($2, $5) }
/* ENDIFF */
;

/* DIFF - Structures */
field_decl_seq:
| field_decl
    { [$1] }
| field_decl SEMI field_decl_seq
    { $1 :: $3 }
;

field_decl:
| /* empty */ IDENT COLON typ
    { ($1, Fimmutable $3) }
| MUTABLE IDENT COLON typ
    { ($2, Fmutable $4) }
;
/* ENDIFF */

typ:
| UNIT
    { Tunit }
| BOOL
    { Tbool }
| INT
    { Tint }
/* DIFF - Références */
| typ REF
    { Tref $1 }
/* ENDIFF */
/* DIFF - Options */
| typ OPTION
    { Toption $1 }
/* ENDIFF */
/* DIFF - Structures */
| IDENT
    { Tident $1 }
/* ENDIFF */
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

simple_expr:
| const
    { mk_node $1 }
| LPAREN expr RPAREN
    { $2 }
| IDENT
    { mk_node (Eident $1) }
/* DIFF - Références */
| GET_REF simple_expr
    { mk_node (Egetref $2) }
/* ENDIFF */
/* DIFF - Structures */
| LBRACE field_def_seq RBRACE
    { mk_node (Estruct $2) }
| field_expr
    { let e, id = $1 in mk_node (Eget (e, id)) }
/* ENDIFF */

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

/* DIFF - Références */
| REF simple_expr
    { mk_node (Eunop (Uref, $2)) }
| expr SET_REF expr
    { mk_node (Esetref ($1, $3)) }
/* ENDIFF */

/* DIFF - Options */
| SOME expr
    { mk_node (Eunop (Usome, $2)) }
| LET SOME IDENT EQUAL expr IN expr
    { mk_node (Eletopt ($3, $5, $7)) }
/* ENDIFF */

/* DIFF - Structures */
| field_expr LT_MINUS expr
    { let e, id = $1 in mk_node (Eset (e, id, $3)) }
/* ENDIFF */

;

/* DIFF - Structures */
field_expr:
| simple_expr DOT IDENT
    { ($1, $3) }
;
/* ENDIFF */

const:
| CONST_UNIT
    { Econst Cunit }
| CONST_BOOL
    { Econst (Cbool $1) }
| CONST_INT
    { Econst (Cint $1) }
/* DIFF - Options */
| NONE
    { Econst Cnone }
/* ENDIFF */
;

/* DIFF - Structures */
field_def_seq:
| field_def
    { [$1] }
| field_def SEMI field_def_seq
    { $1 :: $3 }
;

field_def:
| IDENT EQUAL expr
    { ($1, $3) }
;
/* ENDIFF */

args:
| simple_expr
    { [$1] }
| simple_expr args
    { $1 :: $2 }
;
