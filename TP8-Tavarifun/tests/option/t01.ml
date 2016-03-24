print_int 236 ;;
print_newline () ;;

(* Comparaisons. *)
if None   == Some 8 then print_int 1 else print_int 2 ;;
if None   == None   then print_int 3 else print_int 4 ;;
if Some 9 == Some 9 then print_int 5 else print_int 6 ;;
