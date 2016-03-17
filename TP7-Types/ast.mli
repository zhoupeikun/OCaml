type position = Lexing.position * Lexing.position

type ident = string

type const =
  | Cint of int
  | Cbool of bool
  | Cunit

type unop = Unot | Uminus
type binop =
  | Beq  | Bneq | Blt  | Ble  | Bgt  | Bge
  | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

type node_expr = { expr: expr; pos: position; mutable typ: typ }

and expr =
  | Econst  of const
  | Eident  of ident
  | Eunop   of unop        * node_expr
  | Ebinop  of binop       * node_expr   * node_expr
  | Eif     of node_expr   * node_expr   * node_expr
  | Eletin  of ident       * node_expr   * node_expr
  | Eapp    of ident       * node_expr list
  | Eprint_int of node_expr
  | Eprint_newline of node_expr
  
(* DIFF *)
and typ =
  | Tunknown
  | Tunit
  | Tbool
  | Tint
(* ENDIFF *)

type instr =
  | Icompute of node_expr
(* DIFF *)
  | Ilet     of ident * typ * node_expr
  | Ifun     of bool * ident * ((ident * typ) list) * typ * node_expr
(* ENDIFF *)
      
type prog = instr list
