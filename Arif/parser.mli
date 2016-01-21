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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
