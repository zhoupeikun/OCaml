print_int 9 ;;
print_newline() ;;

(* Déréférencement d'une référence *)
let a : int ref = ref 5 ;;
a := 9 ;;
print_int !a ;;
