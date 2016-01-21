open Ast
open Error

let not_implemented () = failwith "Not implemented"

(* Les expressions de notre langage calculent des valeurs qui sont soit
   des entiers, soit des booléens, soit la valeur unité qui ne représente
   rien.
*)
type value =
  (* À compléter *)
  | Vint of int
  | Vbool of bool
  | Vunit


(* Cette déclaration nous permet de définir des environnements associant
   des valeurs à des chaînes de caractères.

   Le type de ces environnements est noté ['a Env.t], sachant que dans
   dans notre cas le paramètre ['a] sera systématiquement [value].
   Pour manipuler ces environnements, on a notamment les fonctions
   [Env.find : string -> 'a Env.t -> 'a] et
   [Env.add  : string -> 'a -> 'a Env.t -> 'a Env.t],
   ainsi que la constante [Env.empty : 'a Env.t].
   Voir la documentation de la bibliothèque Caml [Map.Make].
*)
module Env = Map.Make(String)


(* La fonction suivante prend une valeur (de type [value]) et en extrait
   le booléen qu'elle contient.
   Elle déclenche une erreur [Interpretation_error] si elle est utilisée
   sur une valeur non adaptée (comme [Vunit]). L'argument [pos] sert en cas
   d'erreur à indiquer la partie du fichier interprété qui a provoqué l'erreur.
   
   get_bool : Ast.position -> value -> bool
*)
let get_bool pos v =
  match v with
    | Vbool b -> b
    | _       -> error Interpretation_error pos

let get_int pos v =
  match v with
  | Vint i -> i
  |_       -> error Interpretation_error pos


(* Voici le cœur de l'interpréteur, qu'il va falloir compléter. Cette fonction
   prend en argument un environnement associant des valeurs à des chaînes de
   caractères ainsi qu'une expression de la forme décrite dans [Ast.mli], et
   renvoie la valeur calculée.

   interpret_expr : value Env.t -> Ast.node_expr -> value
*)
let rec interpret_expr env e =
  match e.expr with
    (* Voici deux cas en exemple, à vous d'écrire les autres ! *)

    | Eunop (Unot, e)   ->
      let b = get_bool e.pos (interpret_expr env e) in
      Vbool (not b)

    | Eprint_newline e ->
      let _ = interpret_expr env e in
      print_newline ();
      Vunit
      
    | Eprint_int e ->
      let b = interpret_expr env e in
      let i = get_int e.pos b in 
      print_int i;
      Vunit

    | Econst c ->begin
      match c with
      | Cint i -> Vint i
      | Cbool b -> Vbool b
      | Cunit -> Vunit
      end 

    | Ebinop (binop, e1, e2) ->
       let a = get_int e1.pos (interpret_expr env e1) and
           b = get_int e2.pos (interpret_expr env e2) in
       Vint (match binop with 
             | Beq
             | Badd -> a + b
             | Bsub -> a - b
             | Bdiv -> a / b
             | Bmul -> a * b
             | _    -> failwith "this system is not supported"
            )
      
    | Eseq l -> interpret_seq env l
      
    | _ -> not_implemented ()

and interpret_seq env l =
  match l with 
  | [] -> Vunit
  | [e] -> interpret_expr env e
  | e :: ll -> let _ = interpret_expr env e in interpret_seq env ll


(* Enfin, la fonction principale, qu'il faut aussi compléter, et qui doit
   appliquer la fonction d'interprétation des expressions à toutes les
   instructions du programme. N'oubliez pas que les instructions peuvent
   agir sur l'environnement !

   interpret_prog : Ast.prog -> unit
*)      
let rec interpret_prog (p : Ast.prog) : unit =
  (* À compléter ! *)
  match p with
  | []    -> ()
  | Icompute e :: pp -> let _ = interpret_expr Env.empty e in interpret_prog pp
  | Ilet (id, e) :: pp -> not_implemented()
