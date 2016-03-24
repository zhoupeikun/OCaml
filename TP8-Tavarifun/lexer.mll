{

  open Lexing
  open Parser
  open Ast
  open Error

  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
	"print_int",     PRINT_INT;
	"print_newline", PRINT_NEWLINE;
	"true",          CONST_BOOL(true);
	"false",         CONST_BOOL(false);
	"not",           NOT;
	"if",            IF;
	"then",          THEN;
	"else",          ELSE;
	"let",           LET;
	"in",            IN;
	"rec",           REC;
	"bool",          BOOL;
	"int",           INT;
	"unit",          UNIT;
	(* DIFF - Références *)
	"ref",           REF;
	(* ENDIFF *)
	(* DIFF - Options *)
	"option",        OPTION;
	(* ENDIFF *)
	(* DIFF - Structures *)
	"type",          TYPE;
	"mutable",       MUTABLE;
	(* ENDIFF *)
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s
	
  let comment_cpt = ref 0

}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | '\n'
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*"
      { incr comment_cpt; comment lexbuf; token lexbuf }

  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | "()"
      { CONST_UNIT }

  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | ";;"
      { EOI }

  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }

  | "=="
      { DOUBLE_EQUAL }
  | "!="
      { NEQ }
  | ">"
      { COMP Bgt }
  | ">="
      { COMP Bge }
  | "<"
      { COMP Blt }
  | "<="
      { COMP Ble }
  | "&&"
      { AND }
  | "||"
      { OR }

  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "="
      { EQUAL }

  | ":"
      { COLON }

  (* DIFF - Références *)
  | "!"
      { GET_REF }
  | ":="
      { SET_REF }
  (* ENDIFF *)

  (* DIFF - Options *)
  | "None"
      { NONE }
  | "Some"
      { SOME }
  (* ENDIFF *)

  (* DIFF - Structures *)
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | ";"
      { SEMI }
  | "."
      { DOT }
  | "<-"
      { LT_MINUS }
  (* ENDIFF *)

  | _
      { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }
  | eof
      { EOF }

and comment = parse
  | "(*"
      { incr comment_cpt; comment lexbuf }
  | "*)"
      { decr comment_cpt; if !comment_cpt > 0 then comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { error (Lexical_error "unterminated comment") (current_pos lexbuf) }
