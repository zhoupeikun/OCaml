open Ast
open Error

(* Informations données par l'environnement de typage. *)
type type_annot =
  | TA_var of typ
  | TA_fun of typ * (typ list)

(* Type de l'environnement de typage. *)
module Env = Map.Make(String)
type  tenv = type_annot Env.t

(* Environnement vide. *)
let empty_env = Env.empty
(* Environnement de base, à compléter pour tenir compte d'éventuelles fonctions
   ou variables primitives, par exemple [print_int] et [print_newline] si
   vous en avez fait des fonctions ordinaires (extension "uniformisation" du
   TP précédent). *)
let base_env = empty_env

(* Ajout à un nœud de l'information de type. *)
let upd_type ne ty = ne.typ <- ty

(* Fonction à appeler pour rapporter des erreurs de types. *)	
let type_error l t1 t2 = error (Type_error(t1, t2)) l

let not_implemented() = failwith "Not implemented"

let rec check_types l ty1 ty2 = 
  match ty1, ty2 with
    | Tunit, Tunit | Tbool, Tbool | Tint, Tint -> ()
    | _, _ -> type_error l ty1 ty2
    
let rec type_expr t_env ne =
  match ne.expr with
    | Econst Cunit    -> upd_type ne Tunit  (* Met à jour le type du nœud. *)
    
    | Econst (Cint _)  ->  upd_type ne Tint
    | Econst (Cbool _) ->  upd_type ne Tbool

    | Eident id  ->
    begin
      try
        match Env.find id t_env with
        | TA_var ty -> upd_type ne ty
        | TA_fun _ -> error(Function_identifier id) ne.pos
      with Not_found -> error (Unknown_identifier id) ne.pos 
    end
    
    | Eunop (op, e) ->
      type_expr t_env e;
      begin
      match op with
        | Unot -> check_types e.pos e.typ Tbool; upd_type ne Tbool
        | Uminus -> check_types e.pos e.typ Tint; upd_type ne Tint
      end
    
    | Eif (e1, e2, e3) ->
      type_expr t_env e1;
      check_types e1.pos Tbool e1.typ;
      type_expr t_env e2;
      type_expr t_env e3;
      check_types e2.pos e2.typ e3.typ;
      upd_type ne e2.typ

      (*
      type_expr t_env e2;
        check_types e2.pos e2.typ Tint; upd_type ne Tbool;
      type_expr t_env e3;
        check_types e3.pos e3.typ Tint; upd_type ne Tint;
      *)

   | Eletin (id, e1, e2) -> 
      type_expr t_env e1;
      let new_env = Env.add id (TA_var e1.typ) t_env in
        type_expr new_env e2;
        upd_type ne e2.typ

 (*  
      type_expr t_env e1;
         begin
            match Env.find id t_env with
            | TA_var ty -> upd_type ne ty
            | TA_fun _ -> error(Function_identifier id) ne.pos
      type_expr t_env e2;      
            match Env.find id t_env with
            | TA_var ty -> upd_type ne ty
            | TA_fun _ -> error(Function_identifier id) ne.pos
  end
  *)  

    |Eprint_int e -> 
      type_expr t_env e;
      check_types e.pos Tint e.typ;
      upd_type ne Tunit

    |Eapp (id, nl) ->
      begin
           try
             match Env.find id t_env with
             | TA_var _ -> () 
             | TA_fun (typ, listTypesArguments) ->
                upd_type ne typ;
                List.iter2 (fun e1 e2 -> type_expr t_env e1; check_types e1.pos e1.typ e2) nl listTypesArguments
           with Not_found -> error(Unknown_identifier id) ne.pos
         end   
    | Ebinop(op, e1, e2) ->
      type_expr t_env e1;
      type_expr t_env e2;
        begin
          match op with
          | ( Beq  | Bneq ) -> 
            check_types e1.pos e1.typ e2.typ;
            upd_type ne Tbool;
          | ( Blt  | Ble  | Bgt  | Bge ) -> 
            check_types e1.pos Tint e1.typ;
            check_types e2.pos Tint e2.typ;
            upd_type ne Tbool
          | ( Badd | Bsub | Bmul | Bdiv ) -> 
            check_types e1.pos Tint e1.typ;
            check_types e2.pos Tint e2.typ;
            upd_type ne Tint
          | ( Band | Bor ) ->
            check_types e1.pos Tbool e1.typ;
            check_types e2.pos Tbool e2.typ;
            upd_type ne Tbool
        end


    | Eprint_newline e ->
      (* Calcule le type de l'expression [e], et met à jour le
	 nœud représentant [e]. *)
      type_expr t_env e;
      (* Vérifie que le type trouvé pour [e] est bien le type
	 attendu [Tunit]. *)
      check_types e.pos Tunit e.typ;
      (* Met à jour le type de l'expression complète. *)
      upd_type ne Tunit

    | _ -> not_implemented()


let type_prog p =
  (* On utilise une fonction auxiliaire, qui type une instruction et
     renvoie l'environnement de typage éventuellement mis à jour. *)
  let rec type_instr_list t_env = function
    | [] -> t_env

    | Icompute(e) :: tl ->
      type_expr t_env e;
      check_types e.pos e.typ Tunit;
      type_instr_list t_env tl

    | Ilet(id, ty, e) :: tl ->
      type_expr t_env e;
      check_types e.pos ty e.typ;
      let new_env = Env.add id (TA_var ty) t_env in 
        type_instr_list new_env tl

    | Ifun(recb, id, args, tyReturn, e) :: tl ->
      let listTypesArguments = List.map snd args in 

      let new_env = List.fold_left
          (fun env (ida, tya) -> Env.add ida (TA_var tya) env)
          t_env args
      in 
        let newnew_env = begin
         if recb then Env.add id (TA_fun (tyReturn, listTypesArguments)) new_env
          else new_env
        end
     in 
      type_expr newnew_env e; 
      check_types e.pos tyReturn e.typ;
      type_instr_list (Env.add id (TA_fun (tyReturn, listTypesArguments)) t_env) tl
    
    | _  -> not_implemented()

  in
  type_instr_list base_env p
       (* Remarque : cette fonction renvoie l'environnement de typage final.
	  Ça peut servir dans l'extension "optimisation : valeurs inutiles". *)
