print_int 123645 ;;
print_newline () ;;

(* Des constructeurs pour les listes. *)
type int_list = { head : int; tail : int_list option } ;;
print_int 1 ;;

let singleton (x: int) : int_list = { head = x; tail = None } ;;
print_int 2 ;;

let cons (hd: int) (tl: int_list) : int_list =
  { head = hd; tail = Some tl }
;;
print_int 3 ;;

let l : int_list = cons 4 (singleton 5) ;;
print_int 6 ;;
print_int l.head ;;
let Some tl = l.tail in print_int tl.head ;;

