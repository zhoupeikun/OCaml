print_int 479 ;;
print_newline () ;;

(* Définition de records. *)
type pair   = { mutable gauche : int; droite : int } ;;
type triple = { mutable tete : int; mutable queue : pair } ;;

let a : pair = { gauche = 3; droite = 4 } ;;
print_int 4 ;;
let b : triple = { tete = 2; queue = a } ;;
print_int 7 ;;
let c : triple = { tete = 0; queue = { gauche = 1; droite = 2 } } ;;
print_int 9 ;;
