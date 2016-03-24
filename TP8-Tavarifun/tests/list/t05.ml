print_int 12314916 ;;
print_newline () ;;

(* Fonction récursive sur une liste : calcul des carrés et affichage. *)
type int_list = { head : int; tail : int_list option } ;;

let singleton (x: int) : int_list = { head = x; tail = None } ;;
let cons (hd: int) (tl: int_list) : int_list =
  { head = hd; tail = Some tl }
;;
print_int 1 ;;

let rec print_list (l: int_list) : unit =
  let a = print_int l.head in
  if l.tail == None
  then ()
  else let Some tl = l.tail in print_list tl
;;
print_int 2 ;;

let square (x: int) : int = x * x ;;
let rec square_list (l: int_list) : int_list =
  if l.tail == None
  then singleton (square l.head)
  else let Some tl = l.tail in cons (square l.head) (square_list tl)
;;
print_int 3 ;;

let l : int_list = cons 1 (cons 2 (cons 3 (singleton 4))) ;;
print_list (square_list l) ;;
