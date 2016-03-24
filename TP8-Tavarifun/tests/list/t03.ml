print_int 121578 ;;
print_newline () ;;

(* Fonction récursive sur une liste : accès à un élément. *)
type int_list = { head : int; tail : int_list option } ;;

let singleton (x: int) : int_list = { head = x; tail = None } ;;
let cons (hd: int) (tl: int_list) : int_list =
  { head = hd; tail = Some tl }
;;
print_int 1 ;;

let rec get_nth (n: int) (l: int_list) : int =
  if n == 0
  then l.head
  else let Some tl = l.tail in get_nth (n-1) tl
;;
print_int 2 ;;

let l : int_list = cons 5 (cons 8 (cons 6 (singleton 78))) ;;
let ll : int_list = cons 1 (cons 2 l) ;;
print_int (get_nth 0 ll) ;;
print_int (get_nth 2 ll) ;;
print_int (get_nth 5 ll) ;;
print_int (get_nth 8 ll) ;;
