print_int 17 ;;
print_newline () ;;

(* Définition de fonctions récursives. *)
let rec f (x: int) : int = x ;;
print_int 1 ;;

let rec g (x: int) : int = f x ;;
print_int 7 ;;
