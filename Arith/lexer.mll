{

  open Lexing
  open Parser
  open Ast
  open Error

  (* Petite fonction utile pour la localisation des erreurs. *)
  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

}



rule token = parse
  (* Le retour à la ligne est traité à part pour aider la localisation. *)
  | '\n'
      { new_line lexbuf; token lexbuf }
      
  (* Cadeau : la fonction print_newline et la constante unité. *)
  | "print_newline"
      { PRINT_NEWLINE }
  | "(*" 
      { comment lexbuf}
  | "()"
      { CONST_UNIT }
  | [' ' '\t' '\n'] 
      { token lexbuf}
  | ";;"
      {EOI}
  | "print_int"
      {PRINT_INT}
  | ['0'-'9'] + as s
      {CONST_INT (int_of_string s)}

      
  (* À compléter. Il faut gérer :
     - les blancs
     - les constantes
     - les noms "print_int" et "print_newline"
     - les symboles
     - les commentaires *)

      
  (* Les autres caractères sont des erreurs lexicales. *)
  | _
      { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }
  (* Fin du fichier. *)
  | eof
      { EOF }

and comment = parse
            | "*)" {token lexbuf}
            | _    {comment lexbuf}
            | eof  {failwith "commentaire non termine"}
