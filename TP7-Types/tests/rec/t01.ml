print_int 12 ;;
print_newline () ;;

(* Appel d'une fonction récursive. *)
let rec f (x: int) : int = if x > 2 then f (x-2) else x ;;

print_int (f 5) ;;
print_int (f 8) ;;
