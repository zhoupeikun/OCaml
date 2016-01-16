type head_elem = Title of string
                 | Style of string
                     
type body_elem =
  A of string * body_elem list
| B of body_elem list
| P of body_elem list
| Ul of body_elem list list
| Text of string

type document = { head : head_elem list;
                  body : body_elem list}


let doc = { head = [Title "My page"; Style "a { text-decoration: underline; }"];
    body = [
      P [Text "Un paragraph de texte(dont un texte";
         B [Text "en gras"]; Text ")" ];
      Ul[ [Text "Premier point"];
          [Text "Le"; A ("http://www.google.com", [Text "Deuxieme Point"]);
           Text "est un lien" ]
         ]
                   ]
          
};;


let has_title l = List.exists(fun e -> match e with 
                             Title _ -> true | _ -> false)


let rec valid_head l =
  match l with
  | [] -> false
  | Title _ :: ll -> not (has_title ll)
  | Style _ :: ll -> valid_head ll
;;

let rec has_link e =
  match e with
  | A _ -> true
  | Text _ -> false
  | B content | P content -> List.exists has_link content
  | Ul l -> List.exists (fun ll -> List.exists has_link ll ) l


let rec valid_link e = 
  match e with 
  | Text _ -> true
  | A (_, s) -> not (List.exists has_link s)
  | B content | P content -> List.for_all valid_link content
  | Ul l -> List.for_all (fun ll -> List.for_all valid_link ll) l
  

let rec boldify_first e =
  match e with 
  | A (href, content) -> A(href, List.map boldify_first content)
  | Text s -> Text s 
  | B content -> B (List.map boldify_first content)
  | P content -> P (List.map boldify_first content)
  | Ul [] -> Ul []
  | Ul (e :: rest) -> Ul ([B (List.map boldify_first e)] :: 
                         List.map (fun l -> List.map boldify_first l) rest)


let rec print_body l =
  List.iter print_body_elem l

and print_body_elem e =
  match e with
  | A (url, content) -> Printf.printf "< a href = '%s' >" url;
                        print_body content;
                        Printf.printf "< /a >\n%!";
  | B (content) -> Printf.printf "<b>";
                   print_body content;
                   Printf.printf "< /b >\n%!";
  | P (content) -> Printf.printf "<p>";
                   print_body content;
                   Printf.printf "< /p >\n%!";
  | Ul elems -> Printf.printf "< ul >";
                List.iter (fun i -> Printf.printf "< li >";
                          print_body i;
                          Printf.printf "< /li >\n%!") elems;
                Printf.printf "< /ul >";
  | Text s -> Printf.printf "%s" s

let rec print_head_elem e = 
  match e with
  | Title s -> Printf.printf "< title >%s< /title >" s
  | Style s -> Printf.printf "< style >\n%s\n</style>" s

let display_html d =
  Printf.printf "<html>\n<head>\n";
  List.iter print_head_elem d.head;
  Printf.printf "</head>\n<body>";
  print_body d.body;
  Printf.printf "</body>\n</html>"
