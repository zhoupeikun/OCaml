{

  open Lexing
  open Parser
  open Ast
  open Error

  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

  let keyword pos =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
	(* Arith *)
	"print_int",     PRINT_INT;
	"print_newline", PRINT_NEWLINE;
	(* Bool *)
	"true",          CONST_BOOL(true);
	"false",         CONST_BOOL(false);
	"not",           NOT;
	"if",            IF;
	"then",          THEN;
	"else",          ELSE;
      ];
    fun s ->
      try Hashtbl.find h s
      with Not_found -> error (Lexical_error s) pos
	
  let comment_cpt = ref 0

}


(* Arith *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

  
rule token = parse
(* Arith *)
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
  | ";"
      { SEMI }
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
(* Bool *)
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
(* Mots-clés *)
  | ident
      { keyword (current_pos lexbuf) (lexeme lexbuf) }
(* Fin *)
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
