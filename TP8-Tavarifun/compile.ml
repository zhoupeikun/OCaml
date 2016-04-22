open Ast
open Mips
open Format

(* Constantes pour la repr茅sentation des donn茅es. *)
let word_size   = 4
let data_size   = word_size
(* DIFF *)
let header_size = word_size
(* ENDIFF *)

let not_implemented() = failwith "Not implemented"

(* Cr茅ation d'une nouvelle 茅tiquette pour les branchements. *)
let new_label =
  let c = ref 0 in
  fun () -> incr c; sprintf "__label__%05i" !c


(* D茅finition de l'environnement. *)
module Env = Map.Make(String)

(* L'environnement contient des variables globales ou locales, et des
   param猫tres de fonctions. Chacun est associ茅 à un num茅ro (son num茅ro
   d'ordre d'apparition parmi les 茅l茅ments visibles à cet endroit).
   Le d茅calage correspondant au num茅ro d'ordre est calcul茅 diff茅remment en
   fonction du type de variable. *)
type var_loc =
  | Global_var of int
  | Local_var  of int
  | Parameter  of int

let get_var_offset = function
  | Global_var k ->   k    * data_size
  | Local_var  k -> - k    * data_size
  | Parameter  k ->  (k+3) * data_size

(* val push : register -> text 
  [push reg] place le contenu de [reg] au sommet de la pile.
  $sp pointe sur l'adresse de la derni猫re case occup茅e. *)
let push reg =
  sub sp sp oi word_size
  @@ sw reg areg (0, sp)

(* val peek : register -> text
  [peek reg] place la valeur en sommet de pile dans [reg] sans d茅piler. *)
let peek reg =
  lw reg areg (data_size - 4, sp)

(* val pop : register -> text
  [pop reg] place la valeur en sommet de pile dans [reg] et d茅pile. *)
let pop reg =
  lw reg areg (0, sp)
  @@ add sp sp oi data_size


(* DIFF *)    

(* Allocation dans le tas d'un nombre de mot donn茅 en argument. *)
(* Utilise les registres [v0], [v1], [s0],
   place au sommet de la pile l'adresse du bloc allou茅. *)
(* Le programme termine si la limite est d茅pass茅e. *)
let malloc size =
     la s0 alab "nxt_loc"   (* Sauvegarde de l'adresse de d茅but du bloc. *)
  @@ lw   v0 areg (0, s0)
  @@ push v0                
  @@ add  v0 v0 oi size     (* Calcul de l'adresse de fin. Arr锚t si d茅passement. *)
  @@ la  v1 alab "max_loc"
  @@ lw  v1 areg (0, v1)
  @@ bgt v0 v1 "out_of_memory"
  @@ sw  v0 areg (0, s0)    (* Allocation confirm茅e : modification de nxt_loc. *)

(* ENDIFF *)

let rec compile_expr env nxt_var e =
  match e.expr with

    | Econst c -> begin
      match c with
	| Cint i      -> li v0 i @@ push v0
	| Cbool true  -> li v0 1 @@ push v0
	| Cunit       -> push zero
	| Cbool false -> push zero
        | Cnone       -> li v0 0 @@ push v0 
	| _           -> not_implemented()
    end

    | Eident id ->
      let var = Env.find id env in
      let offset = get_var_offset var in
      let load = match var with
	| Global_var k -> lw v0 areg (offset, gp)
	| Local_var k  
	| Parameter k -> lw v0 areg (offset, fp)
	in
	  load @@ push v0

    | Egetref e ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ pop v0
      @@ lw a0 areg (header_size, v0)
      @@ push a0

    | Esetref (e1, e2) ->
      let e1_code = compile_expr env nxt_var e1 in
      let e2_code = compile_expr env nxt_var e2 in
      e1_code @@ e2_code
      @@ pop a2
      @@ pop a1
      @@ sw a2 areg (header_size, a1)
      @@ push zero
        

    | Eunop (uref, e) ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ pop a0
      @@ malloc (header_size + data_size)
    (* Header = 1 pour les r茅f茅rences *)
      @@ peek a1 (* r茅cup猫re l'adress *)
      @@ li a2 1
      @@ sw a2 areg (0, a1)
    (* Stockage valeur e *)
      @@ sw a0 areg (header_size, a1)

    | Eunop (op, e) ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ pop v0
      @@ (match op with
	| Unot   -> li v1 1 @@ sub v0 v1 oreg v0
	| Uminus -> sub v0 zero oreg v0
	| _      -> not_implemented()
      ) @@ push v0

    | Eunop (Usome, e) ->
       let e_code = compile_expr env nxt_var e in
       e_code
       @@ pop a0
       @@ malloc (header_size + data_size)
       @@ peek a1
       @@ li a2 2
       @@ sw a2 areg (0, a1)
       @@ sw a0 areg (header_size, a1)

    | Eletopt (id, e1, e2) ->
       let e1_code = compile_expr env nxt_var e1 in
       let e1_code_value =
         e1_code
         @@ pop v0
         @@ lw a0 areg (header_size, v0)
         @@ push a0
       in
       let env2 = Env.add id (Local_var nxt_var) env
       and nct_var2 = nxt_var +1
       in
       let e2_code = compile_expr env2 nxt_var2 e2 in
       let desalloc_code =
         pop v0
         @@ pop zero
         @@ push v0
       in
       e1_code_value
       @@ e2_code
       @@ desalloc_code

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
      (* DIFF *)
      let e1_code = compile_expr env (nxt_var + 1) e1
      (* ENDIFF *)
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
      (* Code pour la d茅sallocation de la variable locale. *)
      let desalloc_code = pop v0 @@ pop zero @@ push v0 in
      (* Calcule [e1] et empile le r茅sultat. *)
      e1_code
      (* Calcule [e2] et empile le r茅sultat, en utilisant [e1]
	 comme variable locale. *)
      @@ e2_code
      (* D茅salloue le r茅sultat de [e1] et d茅place le r茅sultat de [e2]. *)
      @@ desalloc_code

    | Eapp (id, args) ->
      (* On empile les arguments de [a_n] à [a_1].
	 On ex茅cute le corps de la fonction (qui s'occupera de la sauvegarde
	 de [ra] et [fp]).
	 On prend le r茅sultat et on le recopie à la place de [a_n] avant
	 de rendre la main. *)
      let args_code, _ = compile_args env nxt_var args in
      args_code
      @@ jal id
      (* R茅cup猫re le r茅sultat r茅sultat au sommet de la pile. *)
      @@ pop v0
      (* On suppose que la fonction appel茅e a plac茅 le r茅sultat à la place
	 des sauvegardes de [ra] et [fp], et on nettoie les arguments.
	 Potentiellement, on pourrait tout faire faire par l'appel茅e. *)
      @@ add sp sp oi (data_size * List.length args)
      @@ push v0

    | Eprint_int e ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ jal "print_int"
      
    | Eprint_newline e ->
      let e_code = compile_expr env nxt_var e in
      e_code
      @@ jal "print_newline"

    | _ -> not_implemented()


and compile_args env nxt_var args =
  match args with
    | []        -> nop, nxt_var
    | a :: args -> let args_code, nxt_var =
		     compile_args env nxt_var args in
		   let a_code = compile_expr env nxt_var a in
		   args_code @@ a_code, nxt_var + 1


let rec compile_instr_list env nxt_global il =
  match il with
    | []       -> nop, nop, nxt_global

    | Icompute e :: il ->
      let e_code  = compile_expr env 0 e in
      let il_code, il_fun_code, glob = compile_instr_list env nxt_global il in
      e_code @@ pop zero @@ il_code, il_fun_code, glob

    | Ilet (id, ty, e) :: il ->
      let e_code = compile_expr env 0 e in
      let offset = get_var_offset (Global_var nxt_global) in
      (* DIFF *)
      let alloc_code = pop v0 @@ sw v0 areg (offset, gp)
      (* ENDIFF *)
      in
      let env = Env.add id (Global_var nxt_global) env
      and nxt_global = nxt_global + 1
      in
      let il_code, il_fun_code, glob = compile_instr_list env nxt_global il in
      e_code @@ alloc_code @@ il_code, il_fun_code, glob

    | Ifun (_, id, params, ty, e) :: il ->
      let f_env, nb_params =
	List.fold_left (fun (env, nxt_param) (id, ty) ->
	  Env.add id (Parameter nxt_param) env, nxt_param + 1
	) (env, 0) params
      in
      let e_code = compile_expr f_env 0 e in
      let f_code =
	label id
	@@ push ra (* Sauvegarde de [fp] et [ra]. *)
	@@ push fp
	@@ sub fp sp oi data_size
	(* D茅finition du nouveau [fp], qui pointe sur le mot de la pile
	   imm茅diatement sous celui o霉 est stock茅 l'ancien [fp]. *)
	@@ e_code
	@@ pop v0 (* Sauvegarde du r茅sultat. *)
	@@ pop fp
	@@ pop ra
	@@ push v0 (* Restauration du r茅sultat. *)
	@@ jr ra
      in
      let il_code, il_fun_code, glob = compile_instr_list env nxt_global il in
      il_code, il_fun_code @@ f_code, glob

    | _ -> not_implemented()


let init glob =
     sub  sp sp oi (data_size * glob)
  @@ move gp sp
  @@ sub  fp sp oi data_size
  (* DIFF *)
  @@ li a0 1024            (* Appel syst猫me sbrk pour r茅server 1024 octets. *)
  @@ li v0 9
  @@ syscall    
  @@ la a0 alab "nxt_loc"  (* L'appel syst猫me a plac茅 dans v0 l'adresse de d茅but
                              de la zone r茅serv茅e, à mettre dans nxt_loc. *)
  @@ sw v0 areg (0, a0)
  @@ add v0 v0 oi 1024     (* Calcul de max_loc, 1024 octets plus loin. *)
  @@ la a0 alab "max_loc"
  @@ sw v0 areg (0, a0)
  (* ENDIFF *)

(* DIFF *)
let alloc_vars() =
     label "nxt_loc"
  @@ dword [0]
  @@ label "max_loc"
  @@ dword [0]
(* ENDIFF *)

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

  (* DIFF *)
  (* Arr锚t d'urgence en cas de d茅passement de la capacit茅 du tas. *)
  @@ label "out_of_memory"
  @@ la a0 alab "__const_out_of_memory"
  @@ li v0 4
  @@ syscall
  @@ b "end_exec"
  (* ENDIFF *)

(* DIFF *)
let constants () =
     label  "__const_out_of_memory"
  @@ asciiz "out of memory"
(* ENDIFF *)

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
    
    (* DIFF *)
    data =
       comment "Constantes"
    @@ constants ()
    @@ comment "Variables globales pour la gestion du tas"
    @@ alloc_vars ()
  (* ENDIFF *)
  }
