print_int 120 ;;
print_newline () ;;

(* Appel d'une fonction récursive. *)
let rec fact x = if x < 2 then 1 else (x * (fact (x-1))) ;;

let r = fact 5 ;;
print_int r ;;
