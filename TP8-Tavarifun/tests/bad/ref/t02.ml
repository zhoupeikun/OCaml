let a : int ref = ref 1 ;;
let b : int ref = ref 2 ;;
a := b ;;
print_int !a ;;
