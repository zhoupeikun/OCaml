{

  open Lexing
  open Parser
  open Ast
  open Error

  (* Petite fonction utile pour la localisation des erreurs. *)
  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b
(*
   let keywords = Hashtbl.create 97
   let () = List.iter (fun(s,t) -> Hashtbl.add keywords s t)
   		[
   			"boolean", BOOLEAN;
   			"true", TRUE;
   			"false", FALSE;
   			"if", IF;
   			"else", ELSE;
   			"then", THEN;
   			"not", NOT;
   		]
   *)
}



rule token = parse
  (* Le retour à la ligne est traité à part pour aider la localisation. *)

  (*|ident as s 
  		{try Hashtbl.find keywords s with Not_found -> IDENT s }
  *)

  | '\n'
      { new_line lexbuf; token lexbuf }
      
  (* Cadeau : la fonction print_newline et la constante unité. *)
  | "print_newline"
      { PRINT_NEWLINE }
  | "()"
      { CONST_UNIT }

   
  (* À compléter. Il faut gérer :
     - les blancs
     - les constantes
     - les noms "print_int" et "print_newline"
     - les symboles
     - les commentaires *)
   | [' ' '\t' '\n' 'r']+
      {token lexbuf}
   | ['0'-'9']+ as s
      {CONST_INT(int_of_string s)}
   | "print_int"
      { PRINT_INT}
   | ";;"
      {EOI}

  (* Et pour le fragment Arif il y a en plus :
     - les constantes booléennes
     - les mots-clés if, then, else, not
     - les symboles booléens et les comparaisons *)
(*--------------------------------------------------------------------------*)
   |"true" 
   		{ CONST_BOOL(true)}
   |"false"
   		{ CONST_BOOL(false)}

   |"if"
   		{IF}
   |"then"
   		{THEN}
   |"else"
   		{ELSE}
   |"not"{  UNOT }

   | "=="
      { COMP Beq}
   | "!="
      { COMP Bneq}
   | "<="
      { COMP Ble}
   | ">="
      { COMP Bge}
   | "<"
      { COMP Blt}
   | ">"
      { COMP Bgt}
   | "&&"
      { COMP Band}
   | "||"
      { COMP Bor}

  (* Les autres caractères sont des erreurs lexicales. *)
  |"(*" { comment lexbuf }
  |"+"  {PLUS}
  |"-"  {MINUS}
  |"*"  {STAR}
  |"/"  {SLASH}
  |"("  {LPAREN}  
  |")"  {RPAREN}
  | _
      { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }

  (* Fin du fichier. *)
  | eof
      { EOF }

  and comment = parse
    |"*)"  { token lexbuf }
    |_   { comment lexbuf}
    |eof { failwith " le commentaire non termine! " }

