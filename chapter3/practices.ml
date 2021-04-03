let list_1 = [1; 2; 3; 4; 5]
let list_2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let list_3 = [1] @ [2; 3; 4] @ [5]

let rec product acc lst =
  match lst with
  [] -> acc
  | x::xs -> product (acc * x) xs

let rec concat acc lst =
  match lst with
  [] -> acc
  | x::xs -> concat (acc ^ x) xs

let is_first_bigred = function
  [] -> false
  | x::_ -> x = "bigred"

let has_two_or_four_elems lst =
  match lst with
  a::b::[] -> true
  | a::b::c::d::[] -> true
  | _ -> false

let is_first_two_equal lst =
  match lst with
  x::y::_ -> x = y
  | _ -> false

let get_fifth lst = if List.length lst < 5 then 0 else List.nth lst 4

let descend_list lst = List.rev (List.sort Stdlib.compare lst)

let get_last lst = List.nth lst ((List.length lst) - 1)

let any_zeroes lst = List.mem 0 lst

let take n lst =
  let rec aux i acc l =
    match l with
    | x::xs when i < n -> aux (i + 1) (x::acc) xs
    | _rest -> List.rev acc
  in
  aux 0 [] lst

let drop n lst =
  let rec aux i l =
    match l with
    | _x::xs when i < n -> aux (i + 1) xs
    | rest -> rest
  in aux 0 lst