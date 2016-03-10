print_int 720 ;;
print_newline () ;;

(* Appel d'une fonction récursive et variables locales. *)
let rec fact x =
  if x < 2
  then 1
  else (let z = (x - 1) in (x * (fact z))) ;;

let r = fact 6 ;;

print_int r ;;
