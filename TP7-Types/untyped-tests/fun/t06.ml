print_int 44 ;;
print_newline () ;;

(* Variables locales et globales dans une fonction. *)
let a = 3 ;;
let b = 5 ;;
let c = 7 ;;
let f x y =
  let z = b + 1 in
  let b = 2 in
  b*x + z*y;;

print_int (f 1 7) ;;
