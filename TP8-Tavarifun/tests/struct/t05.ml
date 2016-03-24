print_int 15 ;;
print_newline () ;;

(* Partage. *)
type my_ref = { mutable value : int } ;;

type ref_pair = { left: my_ref; right: my_ref } ;;

let r : my_ref = { value = 1 } ;;

let p : ref_pair = { left = r; right = r } ;;

print_int p.right.value ;;

p.left.value <- 5 ;;

print_int p.right.value ;;
