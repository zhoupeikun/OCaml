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


(* Définition de l'environnement. *)
module Env = Map.Make(String)

(* L'environnement contient des variables globales ou locales, et des
   paramètres de fonctions. Chacun est associé à un numéro (son numéro
   d'ordre d'apparition parmi les éléments visibles à cet endroit).
   Le décalage correspondant au numéro d'ordre est calculé différemment en
   fonction du type de variable. *)
type var_loc =
  | Global_var of int
  | Local_var  of int

type venv = var_loc Env.t

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
  lw reg areg (0, sp)
  @@ add sp sp oi data_size

    
(* La fonction de compilation doit prendre en argument, en plus de
   l'expression :
   . l'environnement des variables (pour les deux extensions)
   . le numéro de la prochaine variable locale (pour l'extension des variables
     locales)
   Le type de [compile_expr] doit donc être
   venv -> int -> node_expr -> text
*)

let rec compile_expr e =
  match e.expr with

    | Econst c -> begin
      match c with
	| Cint i      -> li v0 i @@ push v0
	| Cbool true  -> li v0 1 @@ push v0
	| Cunit       -> push zero
	| Cbool false -> push zero
    end

    | Eunop (op, e) ->
      let e_code = compile_expr e in
      e_code
      @@ pop v0
      @@ (match op with
	| Unot   -> li v1 1 @@ sub v0 v1 oreg v0
	| Uminus -> sub v0 zero oreg v0
      ) @@ push v0

    | Ebinop ((Band | Bor) as op, e1, e2) ->
      let e1_code = compile_expr e1
      and e2_code = compile_expr e2
      and lbl_end = new_label ()
      in
      e1_code
      @@ peek v0
      @@ (match op with
	| Band -> beqz v0 lbl_end
	| Bor  -> bnez v0 lbl_end
	| _    -> assert false)
      @@ pop zero
      @@ e2_code
      @@ label lbl_end

    | Ebinop (op, e1, e2) ->
      let e1_code = compile_expr e1
      and e2_code = compile_expr e2
      in
      e2_code
      @@ e1_code
      @@ pop v0
      @@ pop v1
      @@ (match op with
	| Beq  -> seq v0 v0 v1
	| Bneq -> sne v0 v0 v1
	| Blt  -> slt v0 v0 v1
	| Ble  -> sle v0 v0 v1
	| Bgt  -> sgt v0 v0 v1
	| Bge  -> sge v0 v0 v1
	| Badd -> add v0 v0 oreg v1
	| Bsub -> sub v0 v0 oreg v1
	| Bmul -> mul v0 v0 oreg v1
	| Bdiv -> div v0 v0 oreg v1
	| _    -> assert false
      ) @@ push v0
	
    | Eif (cond, e_then, e_else) ->
      let cond_code = compile_expr cond
      and then_code = compile_expr e_then
      and else_code = compile_expr e_else
      and lbl_else  = new_label ()
      and lbl_end   = new_label ()
      in
      cond_code
      @@ pop v0
      @@ beqz v0 lbl_else
      @@ then_code
      @@ b lbl_end
      @@ label lbl_else
      @@ else_code
      @@ label lbl_end

    | Eprint_int e ->
      let e_code = compile_expr e in
      e_code
      @@ jal "print_int"
      
    | Eprint_newline e ->
      let e_code = compile_expr e in
      e_code
      @@ jal "print_newline"

    | _ -> not_implemented()
	

(* Cette fonction de compilation devra prendre deux autres arguments :
   . l'environnement des variables (pour les deux extensions)
   . le numéro de la prochaine variable globale (pour l'extension des variables
     globales)
   Le type de [compile_instr_list] doit donc être
   venv -> int -> instr list -> text
*)

let rec compile_instr_list il =
  match il with
    | []       -> nop

    | Icompute e :: il ->
      let e_code  = compile_expr e in
      let il_code = compile_instr_list il in
      e_code @@ pop zero @@ il_code

    | _ -> not_implemented()


let built_ins () =
  (* Pour [print_int] et [print_newline] on se passe du cadre d'activation. *)
  label "print_int"
  @@ pop a0
  @@ li v0 1
  @@ syscall
  @@ push zero
  @@ jr ra

  @@ label "print_newline"
  @@ pop zero
  @@ li v0 11
  @@ li a0 10
  @@ syscall
  @@ push zero
  @@ jr ra


(* L'appel à [compile_instr_list] doit être mis à jour : il faut lui fournir
   ses deux nouveaux arguments (environnement initial, et numéro à donner à la
   première variable globale).
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
