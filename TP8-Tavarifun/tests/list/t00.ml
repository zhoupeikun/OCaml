print_int 1212346 ;;
print_newline () ;;

(* Définition du type des listes : record récursif. *)
type int_list = { head : int; tail : int_list option } ;;
print_int 1 ;;

let l1 : int_list = { head = 5; tail = None } ;;
let l2 : int_list = { head = 12; tail = Some l1 } ;;
print_int 2 ;;
print_int l2.head ;;
let l3 : int_list = { head = 4; tail = Some { head = 6; tail = None } } ;;
print_int 3 ;;
print_int l3.head ;;
let Some tl = l3.tail in print_int tl.head ;;

