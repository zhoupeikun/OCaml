print_int 3730 ;;
print_newline () ;;

(* Variables locales dans une fonction. *)
let g (b: bool) (x: int) : int =
  let y = x + 1 in
  let z = y * y in
  if b then let t = z + 1 in y + z + t else y + z ;;

print_int (g true 3) ;;
print_int (g false 4) ;;
