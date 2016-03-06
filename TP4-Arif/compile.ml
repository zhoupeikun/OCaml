open Ast
open Mips
open Format



(* Constantes pour la représentation des données. *)
let word_size   = 4
let data_size   = word_size

let not_implemented() = failwith "Not implemented"

(* Création d'une nouvelle étiquette pour les branchements. *)
let new_label =
  let c = ref 0 in
  fun () -> incr c; sprintf "__label__%05i" !c

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
	   (* À compléter avec les autres formes de constantes. *)
       | Cint i      -> li a0 i @@ push a0
       | Cbool b     -> 
          begin match b with
          | true -> li a0 1 @@ push a0
          | false -> li a0 0 @@ push a0
          end
       | _           -> not_implemented()
    end

    (* Pour l'affichage, on calcul la valeur de l'argument, puis on saute au
       fragment de code gérant l'affichage proprement dit. *)
  | Eprint_newline e ->
      let e_code = compile_expr e in
      e_code
      @@ jal "print_newline"

    (* À compléter avec les autres formes d'expressions. *)
  | Eunop(Uminus, e1) ->
    let i1 = (compile_expr e1) in i1 
      @@ pop a1 
      @@ sub a1 zero oreg a1  
      @@ push a1 

  | Eunop(Unot, e1) ->
     let e_code = compile_expr e1 in
     e_code
       @@ pop v0
       @@ li v1 0
       @@ seq v0 v0 v1
       @@ push v0
       

  |Ebinop(op, e1 ,e2)->
      let e1_code = compile_expr e1
      and e2_code = compile_expr e2 in
          e2_code
          @@ e1_code
          @@ pop v0
          @@ pop v1
          @@ ( match op with
           |Badd ->  add v0 v0 oreg v1
           |Bsub ->  sub v0 v0 oreg v1
           |Bmul ->  mul v0 v0 oreg v1
           |Bdiv ->  div v0 v0 oreg v1
           |Beq  ->  seq v0 v0 v1
           |Bgt  ->  sgt v0 v0 v1
           |Bge  ->  sge v0 v0 v1
           |Blt  ->  slt v0 v0 v1
           |Ble  ->  sle v0 v0 v1
           |Band ->  and_ v0 v0 v1
           |Bor  ->  or_ v0 v0 v1
           |_    -> not_implemented ()
             ) @@ push v0


  | Eprint_int se -> 
      let se_code = compile_expr se in 
        se_code
        @@ jal "print_int"

  |Eif (e1,e2,e3) -> 
    let l_else = new_label() in 
    let l_fin = new_label() in
    let e1_code = compile_expr e1 in
    let e2_code = compile_expr e2 in
    let e3_code = compile_expr e3 in 
      e1_code
        @@ pop a0
        @@ beqz a0 l_else
        @@ e2_code
        @@ b l_fin
        @@ label l_else
        @@ e3_code
        @@ label l_fin 

  | _ -> not_implemented()
	


      
(* Les instructions sont calculées l'une après l'autre. *)
let rec compile_instr_list il =
  match il with
    | []       -> nop

    (* À compléter pour le cas [Icompute] et l'itération. *)
     |Icompute e::il ->
      let e_code = compile_expr e in 
      let il_code = compile_instr_list il in
      e_code @@ pop zero @@ il_code
          
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
