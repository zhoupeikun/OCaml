open Ast
open Format
open Lexing

type error = 
  | Lexical_error of string
  | Syntax_error
  | Interpretation_error
  | Structure_identifier of string
  | Function_identifier of string
  | Unknown_identifier of string
  | Not_a_function of string
  | Not_a_struct_type of typ
  | Not_an_option of typ
  | Field_not_found of ident * typ
  | Unknown_field_name of ident
  | Unknown_type of ident
  | Not_mutable of ident
  | Different_identifiers of ident * ident
  | Type_error of typ * typ

exception Error of error * Ast.position

let report_loc fmt file (b,e) =
  if b = dummy_pos || e = dummy_pos then
  fprintf fmt "File \"%s\"\nerror: " file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf fmt "File \"%s\", line %d, characters %d-%d\nerror: " file l fc lc

let tvar_to_string = string_of_int

let rec typ_to_string t =
  match t with
    | Tunknown    -> "unknown"
    | Tunit       -> "unit"
    | Tbool       -> "bool"
    | Tint        -> "int"
    | Tref ty     -> (typ_to_string ty) ^ "ref"
    | Toption ty  -> (typ_to_string ty) ^ "option"
    | Tnone       -> "none"
    | Tident id   -> id

let to_string e =
  match e with
    | Lexical_error s -> sprintf "lexical error: %s" s
    | Syntax_error -> sprintf "syntax error"
    | Interpretation_error -> sprintf "interpretation error"
    | Structure_identifier id -> sprintf "identifier %s denotes a structure type" id
    | Function_identifier id -> sprintf "identifier %s denotes a function" id
    | Unknown_identifier id -> sprintf "unknown identifier %s" id
    | Not_a_function id -> sprintf "identifier %s does not denote a function" id
    | Not_an_option typ -> sprintf "option type expected, which %s is not" (typ_to_string typ)
    | Not_a_struct_type typ -> sprintf "structure type expected, which %s is not" (typ_to_string typ)
    | Field_not_found (id, typ) -> sprintf "field %s not found in structure type %s" id (typ_to_string typ)
    | Unknown_field_name id -> sprintf "unknown field name %s" id
    | Unknown_type id -> sprintf "unknown type name %s" id
    | Not_mutable id -> sprintf "immutable field %s" id
    | Different_identifiers(id1, id2) -> sprintf "identifiers %s and %s do not match" id1 id2
    | Type_error(t1, t2) -> sprintf "this expression should have type %s but has type %s" (typ_to_string t1) (typ_to_string t2)

let print fmt f e p =
  report_loc fmt f p;
  fprintf fmt "%s\n@." (to_string e)

let error e p = raise (Error (e,p))
