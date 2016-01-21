type token =
  | AND
  | COMP of (Ast.binop)
  | CONST_BOOL of (bool)
  | CONST_INT of (int)
  | CONST_UNIT
  | DOUBLE_EQUAL
  | ELSE
  | EOF
  | EOI
  | EQUAL
  | IDENT of (string)
  | IF
  | IN
  | LET
  | LPAREN
  | MINUS
  | NEQ
  | NOT
  | OR
  | PLUS
  | PRINT_INT
  | PRINT_NEWLINE
  | RPAREN
  | SEMI
  | SLASH
  | STAR
  | THEN

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

  open Ast

  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let mk_node e = { expr = e; pos = current_pos() }

# 43 "parser.ml"
let yytransl_const = [|
  257 (* AND *);
  261 (* CONST_UNIT *);
  262 (* DOUBLE_EQUAL *);
  263 (* ELSE *);
    0 (* EOF *);
  264 (* EOI *);
  265 (* EQUAL *);
  267 (* IF *);
  268 (* IN *);
  269 (* LET *);
  270 (* LPAREN *);
  271 (* MINUS *);
  272 (* NEQ *);
  273 (* NOT *);
  274 (* OR *);
  275 (* PLUS *);
  276 (* PRINT_INT *);
  277 (* PRINT_NEWLINE *);
  278 (* RPAREN *);
  279 (* SEMI *);
  280 (* SLASH *);
  281 (* STAR *);
  282 (* THEN *);
    0|]

let yytransl_block = [|
  258 (* COMP *);
  259 (* CONST_BOOL *);
  260 (* CONST_INT *);
  266 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\005\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\006\000\006\000\006\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\005\000\001\000\003\000\001\000\
\001\000\002\000\003\000\003\000\003\000\003\000\002\000\002\000\
\006\000\006\000\002\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\026\000\027\000\025\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\000\000\009\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\015\000\016\000\001\000\003\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\014\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000"

let yysindex = "\017\000\
\053\000\000\000\000\000\000\000\000\000\000\000\072\000\001\255\
\072\000\072\000\072\000\005\255\005\255\000\000\032\000\053\000\
\016\255\000\000\000\000\202\255\028\255\153\255\030\255\019\255\
\245\254\000\000\000\000\000\000\000\000\000\000\000\000\072\000\
\072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
\072\000\033\255\072\000\072\000\000\000\024\000\035\000\035\000\
\245\254\035\000\004\000\245\254\000\000\000\000\000\000\072\000\
\222\255\037\255\182\255\233\255\072\000\000\000\072\000\253\255\
\253\255"

let yyrindex = "\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\
\000\000\000\000\000\000\255\254\000\000\000\000\000\000\000\000\
\045\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\173\255\114\255\127\255\
\068\255\140\255\250\254\091\255\000\000\000\000\000\000\000\000\
\000\000\000\000\047\255\000\000\000\000\000\000\000\000\036\255\
\029\000"

let yygindex = "\000\000\
\000\000\049\000\000\000\252\255\010\000\000\000\249\255"

let yytablesize = 349
let yytable = "\022\000\
\024\000\024\000\025\000\026\000\024\000\024\000\028\000\003\000\
\004\000\005\000\023\000\024\000\040\000\041\000\006\000\024\000\
\024\000\001\000\009\000\024\000\028\000\027\000\028\000\031\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\029\000\
\054\000\055\000\053\000\057\000\059\000\042\000\044\000\058\000\
\045\000\056\000\018\000\018\000\062\000\010\000\010\000\018\000\
\060\000\002\000\010\000\010\000\010\000\064\000\028\000\065\000\
\010\000\018\000\018\000\010\000\010\000\018\000\010\000\010\000\
\030\000\000\000\010\000\010\000\012\000\012\000\010\000\000\000\
\000\000\012\000\012\000\012\000\000\000\000\000\000\000\012\000\
\000\000\000\000\012\000\012\000\000\000\012\000\012\000\000\000\
\000\000\012\000\012\000\011\000\011\000\012\000\000\000\000\000\
\011\000\011\000\011\000\000\000\000\000\000\000\011\000\000\000\
\000\000\011\000\011\000\000\000\011\000\011\000\000\000\000\000\
\011\000\011\000\022\000\022\000\011\000\000\000\000\000\022\000\
\022\000\022\000\000\000\000\000\000\000\022\000\000\000\020\000\
\020\000\022\000\000\000\022\000\020\000\020\000\020\000\022\000\
\022\000\000\000\020\000\022\000\021\000\021\000\020\000\000\000\
\020\000\021\000\021\000\021\000\020\000\020\000\000\000\021\000\
\020\000\032\000\033\000\021\000\000\000\021\000\034\000\000\000\
\000\000\021\000\021\000\000\000\000\000\021\000\000\000\035\000\
\036\000\000\000\037\000\038\000\000\000\023\000\000\000\000\000\
\040\000\041\000\043\000\023\000\023\000\000\000\032\000\033\000\
\023\000\000\000\000\000\034\000\000\000\000\000\023\000\000\000\
\000\000\063\000\023\000\023\000\035\000\036\000\023\000\037\000\
\038\000\000\000\032\000\033\000\039\000\040\000\041\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\036\000\000\000\037\000\038\000\000\000\032\000\033\000\
\039\000\040\000\041\000\034\000\061\000\000\000\000\000\000\000\
\000\000\032\000\033\000\000\000\035\000\036\000\034\000\037\000\
\038\000\000\000\000\000\000\000\063\000\040\000\041\000\035\000\
\036\000\000\000\037\000\038\000\000\000\032\000\033\000\000\000\
\040\000\041\000\034\000\000\000\032\000\033\000\000\000\000\000\
\000\000\034\000\000\000\035\000\036\000\000\000\037\000\038\000\
\000\000\000\000\035\000\036\000\040\000\041\000\038\000\000\000\
\000\000\033\000\000\000\040\000\041\000\034\000\000\000\000\000\
\000\000\000\000\000\000\017\000\017\000\000\000\035\000\036\000\
\017\000\000\000\038\000\000\000\000\000\000\000\000\000\040\000\
\041\000\035\000\017\000\017\000\000\000\038\000\017\000\003\000\
\004\000\005\000\040\000\041\000\000\000\000\000\006\000\007\000\
\000\000\008\000\009\000\010\000\000\000\011\000\000\000\000\000\
\012\000\013\000\003\000\004\000\005\000\000\000\000\000\000\000\
\000\000\006\000\007\000\000\000\021\000\009\000\010\000\000\000\
\011\000\000\000\000\000\012\000\013\000"

let yycheck = "\007\000\
\007\001\008\001\010\000\011\000\009\000\012\001\008\001\003\001\
\004\001\005\001\010\001\018\001\024\001\025\001\010\001\022\001\
\023\001\001\000\014\001\026\001\022\001\012\000\013\000\008\001\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\000\000\
\040\000\041\000\039\000\043\000\044\000\010\001\009\001\044\000\
\022\001\009\001\007\001\008\001\008\001\001\001\002\001\012\001\
\056\000\000\000\006\001\007\001\008\001\061\000\008\001\063\000\
\012\001\022\001\023\001\015\001\016\001\026\001\018\001\019\001\
\016\000\255\255\022\001\023\001\001\001\002\001\026\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\012\001\
\255\255\255\255\015\001\016\001\255\255\018\001\019\001\255\255\
\255\255\022\001\023\001\001\001\002\001\026\001\255\255\255\255\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\255\255\
\255\255\015\001\016\001\255\255\018\001\019\001\255\255\255\255\
\022\001\023\001\001\001\002\001\026\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\012\001\255\255\001\001\
\002\001\016\001\255\255\018\001\006\001\007\001\008\001\022\001\
\023\001\255\255\012\001\026\001\001\001\002\001\016\001\255\255\
\018\001\006\001\007\001\008\001\022\001\023\001\255\255\012\001\
\026\001\001\001\002\001\016\001\255\255\018\001\006\001\255\255\
\255\255\022\001\023\001\255\255\255\255\026\001\255\255\015\001\
\016\001\255\255\018\001\019\001\255\255\001\001\255\255\255\255\
\024\001\025\001\026\001\007\001\008\001\255\255\001\001\002\001\
\012\001\255\255\255\255\006\001\255\255\255\255\018\001\255\255\
\255\255\012\001\022\001\023\001\015\001\016\001\026\001\018\001\
\019\001\255\255\001\001\002\001\023\001\024\001\025\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\016\001\255\255\018\001\019\001\255\255\001\001\002\001\
\023\001\024\001\025\001\006\001\007\001\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\015\001\016\001\006\001\018\001\
\019\001\255\255\255\255\255\255\012\001\024\001\025\001\015\001\
\016\001\255\255\018\001\019\001\255\255\001\001\002\001\255\255\
\024\001\025\001\006\001\255\255\001\001\002\001\255\255\255\255\
\255\255\006\001\255\255\015\001\016\001\255\255\018\001\019\001\
\255\255\255\255\015\001\016\001\024\001\025\001\019\001\255\255\
\255\255\002\001\255\255\024\001\025\001\006\001\255\255\255\255\
\255\255\255\255\255\255\007\001\008\001\255\255\015\001\016\001\
\012\001\255\255\019\001\255\255\255\255\255\255\255\255\024\001\
\025\001\015\001\022\001\023\001\255\255\019\001\026\001\003\001\
\004\001\005\001\024\001\025\001\255\255\255\255\010\001\011\001\
\255\255\013\001\014\001\015\001\255\255\017\001\255\255\255\255\
\020\001\021\001\003\001\004\001\005\001\255\255\255\255\255\255\
\255\255\010\001\011\001\255\255\013\001\014\001\015\001\255\255\
\017\001\255\255\255\255\020\001\021\001"

let yynames_const = "\
  AND\000\
  CONST_UNIT\000\
  DOUBLE_EQUAL\000\
  ELSE\000\
  EOF\000\
  EOI\000\
  EQUAL\000\
  IF\000\
  IN\000\
  LET\000\
  LPAREN\000\
  MINUS\000\
  NEQ\000\
  NOT\000\
  OR\000\
  PLUS\000\
  PRINT_INT\000\
  PRINT_NEWLINE\000\
  RPAREN\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  THEN\000\
  "

let yynames_block = "\
  COMP\000\
  CONST_BOOL\000\
  CONST_INT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr_seq) in
    Obj.repr(
# 56 "parser.mly"
                ( _1 )
# 261 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                 ( [] )
# 267 "parser.ml"
               : 'instr_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr_seq) in
    Obj.repr(
# 61 "parser.mly"
                        ( _1 :: _2 )
# 275 "parser.ml"
               : 'instr_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_seq) in
    Obj.repr(
# 66 "parser.mly"
    ( Icompute (mk_node (Eseq _1)) )
# 282 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_seq) in
    Obj.repr(
# 68 "parser.mly"
    ( Ilet (_2, mk_node (Eseq _4)) )
# 290 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 73 "parser.mly"
    ( mk_node _1 )
# 297 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_seq) in
    Obj.repr(
# 75 "parser.mly"
    ( mk_node (Eseq _2) )
# 304 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
    ( mk_node (Eident _1) )
# 311 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 82 "parser.mly"
    ( _1 )
# 318 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
    ( mk_node (Eunop (Uminus, _2)) )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
    ( mk_node (Ebinop (Badd, _1, _3)) )
# 333 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
    ( mk_node (Ebinop (Bsub, _1, _3)) )
# 341 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
    ( mk_node (Ebinop (Bmul, _1, _3)) )
# 349 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
    ( mk_node (Ebinop (Bdiv, _1, _3)) )
# 357 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 94 "parser.mly"
    ( mk_node (Eprint_int _2) )
# 364 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 96 "parser.mly"
    ( mk_node (Eprint_newline _2) )
# 371 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
    ( mk_node (Eletin (_2, _4, _6)) )
# 380 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
    ( mk_node (Eif (_2, _4, _6)) )
# 389 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
    ( mk_node (Eunop (Unot, _2)) )
# 396 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
    ( mk_node (Ebinop (Beq, _1, _3)) )
# 404 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
    ( mk_node (Ebinop (Bneq, _1, _3)) )
# 412 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
    ( mk_node (Ebinop (_2, _1, _3)) )
# 421 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
    ( mk_node (Ebinop (Band, _1, _3)) )
# 429 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
    ( mk_node (Ebinop (Bor, _1, _3)) )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
    ( Econst Cunit )
# 443 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 123 "parser.mly"
    ( Econst (Cbool _1) )
# 450 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 125 "parser.mly"
    ( Econst (Cint _1) )
# 457 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
    ( [_1] )
# 464 "parser.ml"
               : 'expr_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_seq) in
    Obj.repr(
# 132 "parser.mly"
    ( _1 :: _3 )
# 472 "parser.ml"
               : 'expr_seq))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
