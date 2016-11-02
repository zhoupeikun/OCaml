(*  
  CMSC330 Fall 2016
  This ocaml code reads a C code and properly indents it
  
  compile for debug:
    ocamlc -g Str.cma smallc.ml 
  
  @author: Anwar Mamat
  @date: 10/15/2016
*)

#load "str.cma"

type data_type =
  |Type_Int
;;

(* Use this as your abstract syntax tree *)

type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast (* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
  else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
  Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;        

(* -------------- Your Code Here ----------------------- *)
(*let rec parse_Function lst = (Id "ToDo",lst)*)
let rec parse_Function lst = 
    match (lookahead lst) with
      |(Tok_Int, ls) -> (match (lookahead ls) with
                |(Tok_Main, lss) -> (match (lookahead lss) with 
                            |(Tok_LParen, lsss) -> (
                              match (lookahead lsss) with 
                                 |(Tok_RParen, lssss) -> (
                                    match (lookahead lssss) with 
                                  |(Tok_LBrace, lsssss) -> let (list_method, l) = parse_methodBody lsssss [] in (Fun(Type_Int, "main", List [], list_method), Tok_END)
                                  |_ -> raise (IllegalExpression "main parse exception")
                                 )
                                 |_ -> raise (IllegalExpression "main parse exception")
                              )
                            | _ -> raise (IllegalExpression "main parse exception")
                          )
                |  _ -> raise (IllegalExpression "main parse exception")
              )
      | _ -> raise (IllegalExpression "main parse exception")
    
and parse_methodBody ls list_ast= 
  let (h, t) = lookahead ls in 
  match h with 
    Tok_Int -> let (a, b) = parse_localDec ls in  parse_methodBody b (list_ast@[a]) 
    | Tok_While -> let (a, b) = parse_while ls in parse_methodBody b (list_ast@[a])
    | Tok_If    -> let (a,b) = parse_if ls in parse_methodBody b (list_ast@[a])
  | Tok_Id  id  -> let (a,b) = parse_assign ls in parse_methodBody b (list_ast@[a])
  | Tok_Print -> let (a,b) = parse_print ls in parse_methodBody b (list_ast@[a])
  |Tok_RBrace -> (List list_ast, t)
  | Tok_END -> (List list_ast, ls)
  | _ -> raise (IllegalExpression "methodBody parsing exception")
  
and parse_localDec l = 
    let (h, t) = lookahead l in
  match h with
    Tok_Int -> let (a, b) = lookahead t in 
          match  a with
          Tok_Id id -> let (h1, t1) = lookahead b in
                 match h1 with 
                 Tok_Semi -> (Define(Type_Int, Id id), t1)
          | _ -> raise (IllegalExpression "assigning name parsing exception")
    | _ -> raise (IllegalExpression "assignStatement type parsing exception")

and parse_statement ls = 
  let (h, t) = lookahead ls in
    match h with
  Tok_While -> parse_while ls
  |Tok_If   -> parse_if ls
  |Tok_Id id  -> parse_assign ls
  |Tok_Print -> parse_print ls
  | _ -> raise (IllegalExpression "statement parsing exception")
  
and parse_statements l list_ast = 
  let (h, t) = lookahead l in 
  match h with 
    Tok_While -> let (a1, b1) = parse_while l in 
           parse_statements b1 (list_ast@[a1])
    | Tok_If -> let (a1, b1) = parse_if l in 
           parse_statements b1 (list_ast@[a1])
    | Tok_Id id -> let (a1, b1) = parse_assign l in 
           parse_statements b1 (list_ast@[a1])
    | Tok_Print -> let (a1, b1) = parse_assign l in 
           parse_statements b1 (list_ast@[a1])
    |Tok_RBrace -> (List list_ast, l)
    |_ -> raise(IllegalExpression "parse statement exception")
    
and parse_while l =
  let (h, t) = lookahead l in
    match h with 
      Tok_While -> let (h1, t1) = lookahead t in
             let (a, b) = parse_exp t1 in 
             let (h3, t3) = lookahead b in
             let (h4, t4) = lookahead t3 in
             let (a2, b2) = parse_methodBody t4 [] in
             match (h1, h3, h4) with
              (Tok_LParen, Tok_RParen, Tok_LBrace )->(While(a,a2),b2)
              |_ -> raise (IllegalExpression "while condition parsing exception")
      |_-> raise (IllegalExpression "while statement parsing exception")

and parse_if l = 
  let (h, t) = lookahead l in
    match h with 
      Tok_If -> let (h1, t1) = lookahead t in
           let (a, b) = parse_exp t1 in 
           let (h3, t3) = lookahead b in
           let (h4, t4) = lookahead t3 in
           let (a2, b2) = parse_methodBody t4 [] in
           let (h5, t5) = lookahead b2 in
           match h1, h3, h4 with
            Tok_LParen, Tok_RParen, Tok_LBrace-> match h5 with 
                                Tok_Else -> let (a3, b3) = parse_else b2 in 
                                   (If(a,a2,a3),b3)
                                | _ -> (If(a,a2, List[]),b2)
            |_ -> raise (IllegalExpression "if condition parsing exception")
      |_ -> raise (IllegalExpression "if parsing exception")

and parse_else l = 
  let (h, t) = lookahead l in
    match h with
      Tok_Else -> let (h1, t1) = lookahead t in
            let (a, b) = parse_methodBody t1 [] in
            match h1 with
              Tok_LBrace -> (a,b)
      | _ -> raise(IllegalExpression "else parsing exception")
      
and parse_assign l = 
  let (h, t) = lookahead l in
    match h with 
      Tok_Id id -> let (h1, t1) = lookahead t in
             let (a, b) = parse_exp t1 in
             let (h2, t2) = lookahead b in
             match h1, h2 with
              Tok_Assign, Tok_Semi -> (Assign(Id id, a), t2)
      | _ -> raise(IllegalExpression "assign parsing exception")

and parse_print l = 
  let (h, t) = lookahead l in
    match h with 
      Tok_Print -> let (h1, t1) = lookahead t in 
             let (a, b) = parse_exp t1 in 
             let (h2, t2) = lookahead b in 
             let (h3, t3) = lookahead t2 in 
             match h1, h2, h3 with
              Tok_LParen, Tok_RParen, Tok_Semi -> (Print a, t3)
      | _ -> raise (IllegalExpression "Print parsing exception") 

and parse_exp l = 
  let (a, b) = parse_additive l in
    let (h, t) = lookahead b in
      match h with 
        Tok_Less -> let (a1, b1) = parse_exp t in (Less(a, a1), b1)
        |Tok_Equal -> let (a1,b1) = parse_exp t in (Equal(a,a1),b1)
        |Tok_Greater -> let (a1,b1) = parse_exp t in (Greater(a,a1),b1)
        |Tok_Sum -> let (a1,b1) = parse_exp t in (Sum(a,a1),b1)
        |Tok_Mult -> let (a1,b1) = parse_exp t in (Mult(a,a1),b1)
        |Tok_Pow -> let (a1,b1) = parse_exp t in (Pow(a,a1),b1)
        | _ ->(a, b)

and parse_additive l =
  let (a,b) =  parse_mult l in 
  let (h, t) = lookahead b in
    match h with 
      Tok_Sum -> let (a1, b1) = parse_additive t in (Sum(a,a1),b1)
      | _ -> (a, b)

and parse_mult l = 
  let (a, b) = parse_pow l in 
  let (h, t) = lookahead b in
    match h with 
      Tok_Mult -> let (a1, b1) = parse_mult t in (Mult(a, a1), b1)
      | _ -> (a, b)

and parse_pow l =
  let (a, b) = parse_primary l in
  let (h, t) = lookahead b in
    match h with 
      Tok_Pow -> let (a1, b1) = parse_pow t in (Pow(a, a1), b1)
      | _ -> (a,b)

and parse_primary l =
  let (h, t) =  lookahead l in
    match h with 
      Tok_LParen -> (let (a, b) = parse_exp t in 
              let (h1, t1) = lookahead b in 
              match h1 with
                Tok_RParen -> (Paren a, t1) )
      |Tok_Id id -> (Id id, t)
      |Tok_Num h -> (Num h, t)
      | _  ->  raise (IllegalExpression "parse_primary exception")
;;



(* ------------------------------------------------------*)





exception Error of int ;;




let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
    | Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
    | Tok_String s->s
    | Tok_Assign->"="
     | Tok_Greater->">"
     | Tok_Less->"<"
     | Tok_Equal->"=="
     | Tok_Semi->";"
     | Tok_Main->"main"
     | Tok_LBrace->"{"
     | Tok_RBrace->"}"
     | Tok_Int->"int" 
     | Tok_Float->"float"
     | Tok_Print->"printf"
     | Tok_If->"if"
     | Tok_Else->"else"
     | Tok_While-> "while"
    )

let print_token_list tokens =
  print_string "Input token list = " ;
  List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
  print_endline ""
;;
  




(* -------------- Your Code Here ----------------------- *)
let rec print_tab pos =
  if pos > 0 then (Printf.printf "____"; print_tab (pos-1))
  else ()
;;

let rec pretty_print pos x = 
  match x with 
    Fun(a,b,c,lst) ->  Printf.printf "int main(){\n";
              print_list 1 lst;
              Printf.printf "\n}";
              Printf.printf "\n"

and print_list pos lst = 
  match lst with 
  List [] -> ()
  | List (h::t) -> print_node pos h;
           (match t with 
            [] -> ()
            | _  -> Printf.printf "\n";
                    print_list pos (List t) )
and print_node pos tree =
  match tree with 
    Id id -> print_tab pos; Printf.printf "%s" id
    
    | Num n -> print_tab pos; Printf.printf "%d" n
    
    | Define(a,b) -> print_tab pos; Printf.printf "int "; print_node 0 b; Printf.printf ";"
    
    | Assign(a,b) -> print_tab pos; print_node 0 a;
            Printf.printf " = ";
            print_node 0 b;
            Printf.printf ";"
            
    | List l -> print_list pos (List l)
    
    | Sum(a,b) -> print_tab pos; print_node 0 a;
           Printf.printf " + ";
           print_node 0 b;
           
           
    | Greater(a,b) -> print_tab pos; 
              print_node 0 a; 
              Printf.printf " > ";
              print_node 0 b;
            
    | Equal (a,b) -> print_tab pos; 
               print_node 0 a; 
             Printf.printf " == ";
             print_node 0 b; 
            
    | Less (a,b) -> print_tab pos; 
              print_node 0 a; 
            Printf.printf " < ";
            print_node 0 b; 

    | Mult (a,b) -> print_tab pos; 
            print_node 0 a; 
            Printf.printf " * ";
            print_node 0 b; 

    | Pow (a,b) ->  print_tab pos; 
            print_node 0 a; 
            Printf.printf " ^ ";
              print_node 0 b; 

    | Print a ->  print_tab pos; 
            Printf.printf "printf("; 
            print_node 0 a;
            Printf.printf ");"

    | If (a,b,c) -> print_tab pos; 
            Printf.printf "if("; 
            print_node 0 a;
            Printf.printf "){\n";
            print_node (pos+1) b; 
            Printf.printf "\n";
            print_tab pos;
            (if c = (List []) then  Printf.printf "}"
            else (Printf.printf "}else{\n"; 
              print_node (pos+1) c; 
              Printf.printf "\n"; 
              print_tab pos; 
            Printf.printf "}"))

    | While (a,b) ->print_tab pos; 
            Printf.printf "while(";
            print_node 0 a; Printf.printf "){\n"; 
            print_node (pos+1) b;
            Printf.printf "\n";
            print_tab pos;
            Printf.printf "}"

    | Paren a -> print_tab pos; 
             Printf.printf "("; 
           print_node 0 a;
           Printf.printf ")"

;;

(* ----------------------------------------------------- *)


(*
you can test your parser and pretty_print with following code 
*)


(*
let prg1 = read_lines "main.c";;
let code = List.fold_left (fun x y->x^y) "" prg1;;  
let t = tokenize code;;
let (a,b)=parse_Function t;;

*)