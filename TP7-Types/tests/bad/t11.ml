(* Incohérence BOOL vs INT *)
let f (x: bool) : int = x + 1 ;;
print_int (f true) ;;
