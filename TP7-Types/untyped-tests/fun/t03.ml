print_int 3 ;;
print_newline () ;;

(* Application de plusieurs fonctions. *)
let f x y = 2 * x + y ;;

let g b x y = if not b then x + y else x - y ;;

print_int (f (g true 1 2) 5) ;;
