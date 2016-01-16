let x = 42;;

let x = 42;;
let z =
  let x = 2 in
  let y = 2 + x in
  x * y
    ;;

let x = 42;;
let z =
  let x =
    let x = 2
    and y = 2 + x in
    x * y
      ;;

Let f x =
  let g x =
    if x = 0 then x else x + 2
  in
  let h y =
    if y = 0 then x else y - 2
  in
  g(h x) + g x
;;

let z = f 2;;
