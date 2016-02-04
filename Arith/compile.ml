open Ast
open Mips
open Format



(* Constantes pour la représentation des données. *)
let word_size   = 4
let data_size   = word_size

let not_implemented() = failwith "Not implemented"



(* Les fonctions [push], [peek] et [pop] sont là pour vous aider à manipuler
   la pile. *)
  
(* val push : register -> text 
  [push reg] place le contenu de [reg] au sommet de la pile.
  $sp pointe sur l'adresse de la dernière case occupée. *)
let push reg =
  sub sp sp oi word_size
  @@ sw reg areg (0, sp)

(* val peek : register -> text
  [peek reg] place la valeur en sommet de pile dans [reg] sans dépiler. *)
let peek reg =
  lw reg areg (data_size - 4, sp)

(* val pop : register -> text
  [pop reg] place la valeur en sommet de pile dans [reg] et dépile. *)
let pop reg =
  peek reg
  @@ add sp sp oi data_size



    
(* La fonction de compilation des expressions prend en argument :
   l'expression à compiler. *)
let rec compile_expr e =
  match e.expr with

    | Econst c -> begin
      match c with
	| Cunit       -> push zero
    | Cint i      ->  li v0 i
                     @@ push v0
    | _           -> not_implemented()
    end
	  
	(* À compléter avec les autres formes de constantes. *)
    | Eprint_int se ->
       let se_code = compile_expr se in
       se_code
       @@ jal "print_int" 


    (* Pour l'affichage, on calcul la valeur de l'argument, puis on saute au
       fragment de code gérant l'affichage proprement dit. *)
    | Eprint_newline e ->
      let e_code = compile_expr e in
      e_code
      @@ jal "print_newline"

    | Eprint_int e ->
       let e_code = compile_expr e in 
       e_code
       @@ jal "print_int"


    (* À compléter avec les autres formes d'expressions. *)

    | _ -> not_implemented()
	


      
(* Les instructions sont calculées l'une après l'autre. *)
let rec compile_instr_list il =
  match il with
    | []       -> nop

    (* À compléter pour le cas [Icompute] et l'itération. *)
    | Icompute e::ll ->
       let e_code = compile_expr e in
       let ll_code = compile_instr_list ll in 
       e_code 
         @@ pop zero
         @@ ll_code

    | _ -> not_implemented()



      
(* Fragments de code pour [print_int] et [print_newline]. *)
let built_ins () =
  label "print_newline"
  @@ pop zero
  @@ li v0 11
  @@ li a0 10
  @@ syscall
  @@ push zero
  @@ jr ra

(* À compléter avec le code de [print_int]. *)

  @@ label "print_int"
  @@ pop a0 
  @@ li v0 1
  @@ syscall
  @@ push zero
  @@ jr ra
   

    
(* La compilation du programme produit un code en trois parties :
   1/ Le code principal (label "main")
   2/ Une instruction de fin (label "end_exec")
   3/ Le code des fonctions d'affichage (built_ins_code)
*)
let compile_prog p =
  let main_code = compile_instr_list p in
  let built_ins_code = built_ins () in
  
  { text =
     comment "Code principal"
  @@ label "main"
  @@ main_code
       
  @@ comment "Fin"
  @@ label "end_exec"
  @@ li v0 10
  @@ syscall
       
  @@ comment "Primitives"
  @@ built_ins_code
  ;
    
    data = nop
  }
