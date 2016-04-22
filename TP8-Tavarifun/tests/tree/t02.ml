print_int 12 ;;
print_int 456789 ;;
print_newline () ;;

(* Construction et lecture d'un arbre de recherche. *)
type int_list = { head: int; tail: int_list option } ;;
type bs_tree  = { elt: int;
		  mutable left: bs_tree option;
		  mutable right: bs_tree option } ;;

let mk_leaf (elt: int) : bs_tree = { elt = elt; left = None; right = None } ;;

let rec bs_insert (elt: int) (bst: bs_tree) : unit =
  if elt < bst.elt
  then if bst.left == None
    then bst.left <- Some (mk_leaf elt)
    else let Some lbst = bst.left in bs_insert elt lbst
  else if bst.right == None
  then bst.right <- Some (mk_leaf elt)
  else let Some rbst = bst.right in bs_insert elt rbst
;;

let rec add_list_to_bst (l: int_list) (bst: bs_tree) : unit =
  let a = bs_insert l.head bst in
  if l.tail == None
  then ()
  else let Some tl = l.tail in add_list_to_bst tl bst
;;

let mk_bst (l: int_list) : bs_tree =
  let bst = mk_leaf l.head in
  if l.tail == None
  then bst
  else (let Some tl = l.tail in add_list_to_bst tl bst;
       bst)
;;

let l : int_list = {
  head = 5; tail = Some {
    head = 8; tail = Some {
      head = 6; tail = Some {
	head = 4; tail = Some {
	  head = 7; tail = Some {
	    head = 9; tail = None }}}}}} ;;
print_int 1 ;;

let bst: bs_tree = mk_bst l ;;
print_int 2 ;;

let rec infix (t: bs_tree) : unit =
  let a =
  if t.left != None
  then let Some l = t.left in infix l
  else ();
  in
  let b =
  print_int t.elt;
  in
  if t.right != None
  then let Some r = t.right in infix r
  else ()
;;

infix bst ;;
