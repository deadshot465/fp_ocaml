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

(** [unimodal] not yet implemented. *)
(** [powerset] not yet implemented. *)

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

let insert k v d = (k, v)::d
let rec lookup k = function
[] -> None
| (k', v)::t -> if k = k' then Some(v) else lookup k t

let assoc_list = insert 1 "one" []
let assoc_list = insert 2 "two" assoc_list
let assoc_list = insert 3 "three" assoc_list
let position_2 = lookup 2 assoc_list
let position_4 = lookup 4 assoc_list

type suit = Spade | Heart | Diamond | Club
type rank = int
type card = { suit: suit; rank: rank }

let club_ace = { suit = Club; rank = 1 }
let heart_queen = { suit = Heart; rank = 12 }
let diamond_two = { suit = Diamond; rank = 2 }
let spade_seven = { suit = Spade; rank = 7 }

let sign (x: int) = if x > 0 then `Pos
else if x < 0 then `Neg
else `Zero

let quadrant = fun (x, y) ->
  match (sign x, sign y) with
  (`Pos, `Pos) -> Some(`I)
  | (`Neg, `Pos) -> Some(`II)
  | (`Neg, `Neg) -> Some(`III)
  | (`Pos, `Neg) -> Some(`IV)
  | _ -> None

let quadrant_when = function
  (x', y') when x' > 0 && y' > 0 -> Some(`I)
  | (x', y') when x' < 0 && y' > 0 -> Some(`II)
  | (x', y') when x' < 0 && y' < 0 -> Some(`III)
  | (x', y') when x' > 0 && y' < 0 -> Some(`IV)
  | _ -> None

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let t = Node(4,
  Node(2,
    Node(1, Leaf, Leaf),
    Node(3, Leaf, Leaf)
    ),
  Node(5,
    Node(6, Leaf, Leaf),
    Node(7, Leaf, Leaf)
    )
  )

let rec size = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let depth tree =
  match tree with
  Leaf -> 0
  | Node (_, l, r) -> max (size l) (size r)

let shape tree_a tree_b =
  match (tree_a, tree_b) with
  (Leaf, Leaf) -> true
  | (Node(_, l, r), Node(_, l', r')) -> ((depth l = depth l') && (size l = size l')) && ((depth r = depth r') && (size r = size r'))
  | _ -> false

let rec max_number_in_list = function
  [] -> 0
  | x::xs -> max x (max_number_in_list xs)

let rec list_max = function
  [] -> failwith "list_max"
  | lst -> max_number_in_list lst

let rec list_max_string = function
  [] -> print_endline "empty"
  | lst -> print_endline @@ string_of_int @@ max_number_in_list lst

(** [is_bst] not yet implemented. *)