print_int 5 ;;
print_newline () ;;

(* Récupérer une valeur dans une option. *)
let x : int option = Some 5 ;;
let Some y = x in print_int y ;;
