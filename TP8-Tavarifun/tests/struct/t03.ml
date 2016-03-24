print_int 3355 ;;
print_newline () ;;

(* Écriture. *)
type pair   = { mutable gauche : int; droite : int } ;;
type triple = { tete : int; mutable queue : pair } ;;

let a : pair = { gauche = 3; droite = 4 } ;;
let b : triple = { tete = 2; queue = a } ;;

print_int a.gauche ;;
print_int b.queue.gauche ;;
a.gauche <- 5 ;;
print_int a.gauche ;;
print_int b.queue.gauche ;;
