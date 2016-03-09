%{

  open Ast

  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let mk_node e = { expr = e; pos = current_pos() }

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
%token <string> IDENT
%token IF
%token LPAREN
%token MINUS
%token NEQ
%token NOT
%token OR
%token PLUS
%token PRINT_INT
%token PRINT_NEWLINE
%token RPAREN
%token SEMI
%token SLASH
%token STAR
%token THEN
%token LET
%token IN
%token EQUAL

%nonassoc ELSE
%right EQUAL IN
%left OR
%left AND
%left COMP NEQ DOUBLE_EQUAL
%left PLUS MINUS
%left STAR SLASH
%nonassoc NOT

%start prog
%type <Ast.prog> prog

%%

/* Arith */
prog:
| instr_seq EOF { $1 }
;

/* Arith */
instr_seq:
| /* empty */    { [] }
| instr instr_seq       { $1 :: $2 }
;

instr:
/* Arith */
| expr EOI
       { Icompute $1 }
| LET IDENT EQUAL expr EOI
      { Ilet ($2, $4) }
;

simple_expr:
/* Arith */
| const
    { mk_node $1 }
| LPAREN expr RPAREN
    { $2 }
| IDENT
    { mk_node (Eident($1))}

expr:
/* Arith */
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
| PRINT_INT simple_expr
    { mk_node (Eprint_int $2) }
| PRINT_NEWLINE simple_expr
    { mk_node (Eprint_newline $2) }

/* Bool */
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
    { mk_node (Eletin ($2, $4, $6))}
;


const:
| CONST_UNIT
    { Econst Cunit }
| CONST_BOOL
    { Econst (Cbool $1) }
| CONST_INT
    { Econst (Cint $1) }
;
