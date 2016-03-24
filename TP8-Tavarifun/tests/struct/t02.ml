print_int 324 ;;
print_newline () ;;

(* Lecture. *)
type pair   = { mutable gauche : int; droite : int } ;;
type triple = { tete : int; mutable queue : pair } ;;

let a : pair = { gauche = 3; droite = 4 } ;;
let b : triple = { tete = 2; queue = a } ;;

print_int a.gauche ;;
print_int b.tete ;;
print_int b.queue.droite ;;
