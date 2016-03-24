print_int 15234 ;;
print_newline () ;;

(* Définition d'une structure d'arbre. *)
type tree = { elt: int; left: tree option; right: tree option } ;;
print_int 1 ;;

let t : tree =
  { elt = 7;
    left = Some { elt = 9;
		  left = None;
		  right = None } ;
    right = None }
;;
print_int 5 ;;

let mk_node_aux (x: int) (l: tree option) (r: tree option) : tree =
  { elt = x; left = l; right = r }
;;
print_int 2 ;;
let mk_leaf (x: int) : tree =
  mk_node_aux x None None
;;
let mk_node (x: int) (l: tree) (r: tree) : tree =
  mk_node_aux x (Some l) (Some r)
;;
let mk_node_left (x: int) (t: tree) : tree =
  mk_node_aux x (Some t) None
;;
let mk_node_right (x: int) (t: tree) : tree =
  mk_node_aux x None (Some t)
;;
print_int 3 ;;

let tt : tree =
  mk_node 3476 (mk_node_right 87 (mk_leaf 987)) (mk_leaf 128)
;;
print_int 4 ;;
