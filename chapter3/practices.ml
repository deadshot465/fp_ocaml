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

(** unimodal not implemented. *)
(** powerset not implemented. *)

let rec print_int_list = function
| [] -> ()
| h::t -> print_int h;
          print_newline();
          print_int_list t

let print_int_list' lst =
  List.iter (fun x -> print_int x; print_newline()) lst

type student = { first_name: string; last_name: string; gpa: float }

let nick = { first_name = "Nick"; last_name = "Chang"; gpa = 4.5 }
let get_student_name s = (s.first_name, s.last_name)
let make_student first_name last_name gpa = { first_name = first_name; last_name = last_name; gpa = gpa }

type poketype = Normal | Fire | Water
type pokemon = { name: string; hp: int; ptype: poketype }
let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

let safe_hd lst =
  match lst with
  [] -> None
  | x::xs -> Some(x)

let safe_tl lst =
  match lst with
  [] -> None
  | x::xs -> Some(xs)

let rec max_hp lst = match lst with
  [] -> None
  | x::xs -> match max_hp xs with
    None -> Some(x)
    | Some(y) -> if y.hp > x.hp then Some(y) else Some(x)

let is_valid_date date =
  match date with
  (y, m, d) -> if y > 1 && (m > 0 && m < 13) && d > 0 then
    match m with
    2 -> d < 29
    | 4 | 6 | 9 | 11 -> d < 31
    | _ -> d < 32
  else false

let is_before date_1 date_2 =
  if is_valid_date date_1 && is_valid_date date_2 then
    match date_1 with
    (y, m, d) -> match date_2 with
      (y', m', d') -> (y < y') || (m < m') || (d < d')
  else failwith "Input contains invalid dates."

let rec earliest lst =
  match lst with
  [] -> None
  | x::xs ->
    match earliest xs with
    None -> Some(x)
    | Some(y) -> if is_before y x then Some(y) else Some(x)