print_int 35 ;;
print_newline () ;;

(* Application d'une fonction constante. *)
let f x = 3 ;;

print_int (f 1) ;;

let g x y = false ;;

if g true 2 then print_int 4 else print_int 5 ;;
