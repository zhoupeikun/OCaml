print_int 3 ;;
print_newline () ;;

(* Application de plusieurs fonctions. *)
let f (x: int) (y: int) : int = 2 * x + y ;;

let g (b: bool) (x: int) (y: int) : int = if not b then x + y else x - y ;;

print_int (f (g true 1 2) 5) ;;
