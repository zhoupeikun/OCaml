print_int 1 ;;
print_int 5678 ;;
print_int 2 ;;
print_int 6758 ;;
print_newline () ;;

(* Parcours d'arbre. *)
type tree = { elt: int; left: tree option; right: tree option } ;;

let t : tree =
  { elt   = 5;
    left  = Some { elt   = 6;
		   left  = None;
		   right = Some { elt   = 7;
			 	  left  = None;
				  right = None } };
    right = Some { elt   = 8;
		   left  = None;
		   right = None } }
;;
print_int 1 ;;

let rec prefix (t: tree) : unit =
  let a = print_int t.elt in
  let b = 
  if t.left != None
  then let Some l = t.left in prefix l
  else ();
  in
  if t.right != None
  then let Some r = t.right in prefix r
  else ()
;;

prefix t ;;
print_int 2 ;;

let rec infix (t: tree) : unit =
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

infix t ;;
