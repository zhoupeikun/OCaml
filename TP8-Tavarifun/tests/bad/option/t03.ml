let x : int option = Some 1 ;;
let Some y = x in if y == None then print_int 2 else print_int y ;;
