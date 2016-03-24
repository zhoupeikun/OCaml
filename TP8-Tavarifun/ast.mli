type position = Lexing.position * Lexing.position

type ident  = string
(* DIFF - Structures *)
type is_mut = bool
(* ENDIFF *)

type const =
  | Cint of int
  | Cbool of bool
  | Cunit
  (* DIFF - Options *)
  | Cnone
  (* ENDIFF *)

(* DIFF - Références - Options *)
type unop = Unot | Uminus | Uref | Usome
(* ENDIFF *)
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
  (* DIFF - Références *)
  | Egetref of node_expr
  | Esetref of node_expr   * node_expr
  (* ENDIFF *)
  (* DIFF - Options *)
  | Eletopt of ident       * node_expr   * node_expr
  (* ENDIFF *)
  (* DIFF Structures *)
  | Estruct of (ident * node_expr) list
  | Eget    of node_expr   * ident
  | Eset    of node_expr   * ident       * node_expr
  (* ENDIFF *)
  | Eapp    of ident       * node_expr list
  | Eprint_int of node_expr
  | Eprint_newline of node_expr
  
and typ =
  | Tunknown
  | Tunit
  | Tbool
  | Tint
  (* DIFF - Références *)
  | Tref    of typ
  (* ENDIFF *)
  (* DIFF - Options *)
  | Toption of typ
  | Tnone
  (* ENDIFF *)
  (* DIFF - Structures *)
  | Tident  of string
  (* ENDIFF *)

(* DIFF - Structures *)
type field_typ =
  | Fmutable   of typ
  | Fimmutable of typ
(* ENDIFF *)

type instr =
  | Icompute of node_expr
  | Ilet     of ident * typ * node_expr
  | Ifun     of bool * ident * ((ident * typ) list) * typ * node_expr
  (* DIFF - Structures *)
  | Istruct  of ident * (ident * field_typ) list
  (* ENDIFF *)

type prog = instr list
