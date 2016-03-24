print_int 1246 ;;
print_newline () ;;

(* Fonction récursive sur une liste : longueur. *)
type int_list = { head : int; tail : int_list option } ;;

let singleton (x: int) : int_list = { head = x; tail = None } ;;
let cons (hd: int) (tl: int_list) : int_list =
  { head = hd ; tail = Some tl }
;;
print_int 1 ;;

let rec length (l: int_list) : int =
  if l.tail == None
  then 1
  else let Some tl = l.tail in 1 + length tl
;;
print_int 2 ;;

let l : int_list = cons 5 (cons 8 (cons 6 (singleton 78))) ;;
print_int (length l) ;;

let ll : int_list = cons 1 (cons 2 l) ;;
print_int (length ll) ;;

