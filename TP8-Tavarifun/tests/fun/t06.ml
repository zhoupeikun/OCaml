print_int 44 ;;
print_newline () ;;

(* Variables locales et globales dans une fonction. *)
let a : int = 3 ;;
let b : int = 5 ;;
let c : int = 7 ;;
let f (x: int) (y: int) : int =
  let z = b + 1 in
  let b = 2 in
  b*x + z*y;;

print_int (f 1 7) ;;
