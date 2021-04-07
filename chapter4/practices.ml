let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

let rec repeat_no_tr f n x = if n = 0 then x else f (repeat_no_tr f (n - 1) x)
let rec repeat_tr f n x = if n = 0 then x else repeat_tr f (n - 1) (f x)

let product_left lst = List.fold_left ( *. ) 1.0 lst
let product_right lst = List.fold_right ( *. ) lst 1.0

let clip n = if n < 0 then 0
else if n > 10 then 10
else n

let cliplist_map lst = List.map clip lst
let rec cliplist_rec lst =
  match lst with
  [] -> lst
  | x::xs -> (clip x)::(cliplist_rec xs)

let (--) i j =
  let rec from i j l =
    if i > j then l
    else from i (j - 1) (j::l)
  in from i j []

let cube x = x * (square x)
let is_odd x = x mod 2 = 1

let sum_cube_odd n = List.fold_left (+) 0 @@ List.map cube @@ List.filter is_odd (0 -- n)

let sum_cube_odd_pipeline n =
  0 -- n
  |> List.filter is_odd
  |> List.map cube
  |> List.fold_left (+) 0

let rec exists_rec predicate lst =
  match lst with
  [] -> false
  | x::xs -> if predicate x then true else exists_rec predicate xs

let exists_fold predicate lst = List.fold_left (fun a x -> if predicate x then true else a) false lst

let exists_lib predicate lst =
  lst
  |> List.map predicate
  |> List.filter (fun x -> x)
  |> (<>) []

let budget_fold_left budget expenses = List.fold_left (-) budget expenses
let budget_fold_right budget expenses = budget - (List.fold_right (+) expenses 0)
let rec budget_rec budget expenses =
  match expenses with
  [] -> budget
  | x::xs -> budget_rec (budget - x) xs

let uncurried_append (lst_a, lst_b) = List.append lst_a lst_b
let uncurried_compare (a, b) = Char.compare a b
let uncurried_max (a, b) = max a b

let uncurry f (x, y) = f x y

let uncurried_nth = uncurry List.nth
let uncurried_append' = uncurry List.append
let uncurried_compare' = uncurry Char.compare
let uncurried_max' = uncurry max

let curry f x y = f (x, y)

let curried_nth = curry uncurried_nth
let curried_append = curry uncurried_append'
let curried_compare = curry uncurried_compare'
let curried_max = curry uncurried_max'

(** terse product: do we really need to make the product function on line 10 and 11 terser? *)

(** map composition not yet implemented. *)

let greater_than_three lst = List.filter (fun x -> String.length x > 3) lst
let add_one_point_zero lst = List.map (fun x -> x +. 1.0) lst
let join strs sep = List.fold_left (fun acc x -> acc ^ sep ^ x) (List.hd strs) (List.tl strs)

(** tree map *)
(** association list keys *)
(** valid matrix *)
(** row vector add *)
(** matrix add *)
(** matrix multiply *)
