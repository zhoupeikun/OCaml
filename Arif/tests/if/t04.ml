print_int 12458;;
print_newline () ;;

(* Ordre d'évaluation. *)
if (let x = print_int 1 in true) && (let x = print_int 2 in false)
then print_int 3
else print_int 4
;;
if (let x = print_int 5 in false) && (let x = print_int 6 in true)
then print_int 7
else print_int 8
;;
