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
  | Parameter  of int

let get_var_offset = function
  | Global_var k ->   k * data_size
  | Local_var  k -> - k * data_size
  | _            -> not_implemented()

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

    
(* La fonction de compilation prend en argument, en plus de l'expression :
   . l'environnement des variables,
   . le numéro de la prochaine variable locale. *)

let rec compile_expr env nxt_var e =
  match e.expr with

    | Econst c -> begin
      match c with
	| Cint i      -> li v0 i @@ push v0
	| Cbool true  -> li v0 1 @@ push v0
	| Cunit       -> push zero
	| Cbool false -> push zero
    end

    | Eident id ->
      let var = Env.find id env in
      let offset = get_var_offset var in
      let load = match var with
	| Global_var k -> lw v0 areg (offset, gp)
	| Local_var k  -> lw v0 areg (offset, fp)
	| _ -> not_implemented()
	in
	load @@ push v0

    | Eunop (op, e) ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ pop v0
      @@ (match op with
	| Unot   -> li v1 1 @@ sub v0 v1 oreg v0
	| Uminus -> sub v0 zero oreg v0
      ) @@ push v0

    | Ebinop ((Band | Bor) as op, e1, e2) ->
      let e1_code = compile_expr env nxt_var e1
      and e2_code = compile_expr env nxt_var e2
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
      let e1_code = compile_expr env nxt_var e1
      and e2_code = compile_expr env nxt_var e2
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
      let cond_code = compile_expr env nxt_var cond
      and then_code = compile_expr env nxt_var e_then
      and else_code = compile_expr env nxt_var e_else
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

    | Eletin (id, e1, e2) ->
      let e1_code  = compile_expr env nxt_var e1
      and env2     = Env.add id (Local_var nxt_var) env
      and nxt_var2 = nxt_var + 1
      in
      let e2_code = compile_expr env2 nxt_var2 e2 in
      (* Code pour la désallocation de la variable locale. *)
      let desalloc_code = pop v0 @@ pop zero @@ push v0 in
      (* Calcule [e1] et empile le résultat. *)
      e1_code
      (* Calcule [e2] et empile le résultat, en utilisant [e1]
	 comme variable locale. *)
      @@ e2_code
      (* Désalloue le résultat de [e1] et déplace le résultat de [e2]. *)
      @@ desalloc_code

    | Eprint_int e ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ jal "print_int"
      
    | Eprint_newline e ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ jal "print_newline"

    | _ -> not_implemented()
	

let rec compile_instr_list env nxt_global il =
  match il with
    | []       -> nop, nop, nxt_global

    | Icompute e :: il ->
      let e_code  = compile_expr env 0 e in
      let il_code, il_fun_code, glob = compile_instr_list env nxt_global il in
      e_code @@ pop zero @@ il_code, il_fun_code, glob

    | Ilet (id, e) :: il ->
      let e_code = compile_expr env 0 e in
      let offset = get_var_offset (Global_var nxt_global) in
      let alloc_code = sw v0 areg (offset, gp)
      in
      let env = Env.add id (Global_var nxt_global) env
      and nxt_global = nxt_global + 1
      in
      let il_code, il_fun_code, glob = compile_instr_list env nxt_global il in
      e_code @@ alloc_code @@ il_code, il_fun_code, glob

    | _ -> not_implemented()


let init glob =
     sub  sp sp oi (data_size * glob)
  @@ move gp sp
  @@ sub  fp sp oi data_size

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

let compile_prog p =
  let main_code, fun_code, glob = compile_instr_list Env.empty 0 p in
  let init_code = init glob in
  let built_ins_code = built_ins () in
  { text =
     comment "Initialisation"
  @@ init_code
  @@ comment "Code principal"
  @@ label "main"
  @@ main_code
  @@ comment "Fin"
  @@ label "end_exec"
  @@ li v0 10
  @@ syscall
  @@ comment "Fonctions"
  @@ fun_code
  @@ comment "Primitives"
  @@ built_ins_code
  ;
    
    data = nop
  }
