print_int 120 ;;
print_newline () ;;

(* Appel d'une fonction récursive. *)
let rec fact (x: int) : int = if x < 2 then 1 else (x * (fact (x-1))) ;;

let r : int = fact 5 ;;
print_int r ;;
