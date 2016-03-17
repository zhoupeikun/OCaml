print_int 59 ;;
print_newline () ;;

(* Application d'une fonction. *)
let f (x: int) : int = x + 3 ;;
let a : int = f 2 ;;
print_int a ;;
print_int (f 6) ;;
