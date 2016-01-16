let rec longueur list =
  match list with
  | [] -> 0
  | _ :: r -> 1 + longueur r
    ;;

(*rec avec un accumulateur*)
let longueur_tail l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | _ :: r -> loop r (acc + 1)
  in
loop l 0
     


let rec applique f l =
  match l with
  | [] -> []
  | a :: r -> (f a) :: (applique f r)
  

(*reverse naive*)
let rec reverse l =
  match l with
  | [] -> []
  | a :: r -> (reverse r) @ [a]
    ;;

(*reverse plus efficace*)
let renverse l =
  let rec loop acc l =
    match l with
    | [] -> acc
    | a :: r -> loop (a :: acc) r in
  loop [] l
    ;;

(*List.rev List.map List.length List.assoc *)


let rec trouve l k =
  match l with
  | [] -> raise Not_found
  | (a, b) :: r ->
    if a = k then b else trouve r k
      ;;
      


(* Tri sur les listes *)

let rec inserse1 e l =
  match l with
  | [] -> [e]
  | x :: s ->
    if x < e then x :: (inserse1 e s)
    else e :: l
      ;;

let rec inserse2 f e l =
  match l with
  | [] -> [e]
  | x :: s ->
    if f x e then x :: (inserse2 f e s)
    else e :: l
      ;;

let rec trier f l =
  match l with
  | [] -> []
  | x :: s ->
    let g = trier f s in
    inserse2 f x g
      ;;

let test = trier (fun x y -> x < y) [1;5;3;4];;

(*Tri rapide*)
let rec partage e l = 
  match l with 
  | [] -> [], []
  | x :: s -> 
     let l1,l2 = partage e s in
     if x < e then x :: l1,l2
     else l1, x :: l2

