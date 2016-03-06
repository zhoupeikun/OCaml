type position = Lexing.position * Lexing.position

type const =
  | Cint of int
  | Cbool of bool
  | Cunit

type unop = Unot | Uminus
type binop =
  | Beq  | Bneq | Blt  | Ble  | Bgt  | Bge
  | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

type node_expr = { expr: expr; pos: position }

and expr =
  | Econst  of const
  | Eunop   of unop        * node_expr
  | Ebinop  of binop       * node_expr   * node_expr
  | Eif     of node_expr   * node_expr   * node_expr
  | Eprint_int of node_expr
  | Eprint_newline of node_expr
      

type instr =
  | Icompute of node_expr
      
type prog = instr list
