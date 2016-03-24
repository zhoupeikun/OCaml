print_int 1035 ;;
print_newline () ;;

(* Une fonction dans une fonction. *)
let f (x: int) (y: int) : int = 2 * x + y ;;
let g (b: bool) (x: int) : int =
  if b
  then f x (x + 1)
  else f (x + 1) x + f (x + 2) (x + 5) ;;
print_int (g true 3) ;;
print_int (g false 4) ;;
