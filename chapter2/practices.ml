let forty_two = 7 * (1 + 2 + 3)
let string_concat = "CS " ^ string_of_int 3110
let multiply_result = 42 * 10
let float_divide = 3.14 /. 2.0
let seventh_power = 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2
let structural_equality = 42 = 42
let physical_equality = "hi" == "hi"
let assert_1 = assert true
let assert_2 = assert (2110 <> 3110)
let if_expression = if 2 > 1 then 42 else 7
let double x = x * 2
let cube x = x *. x *. x

let sign x = if x > 0 then 1
else if x < 0 then -1 else 0

let area_of_circle radius = 0.5 *. 3.14 *. (radius *. radius)

let root_mean_square x y = sqrt(((x *. x) +. (y *. y)) /. 2.0)

let is_valid_date d m = if (d < 1) || (d > 31) then false
else if (m = "Feb") then (d < 29)
else if (m = "Apr") || (m = "Jun") || (m = "Sep") || (m = "Nov") then (d < 31)
else (d < 32)

let rec slow_fib n = if (n = 1) then 1
else if (n = 2) then 1
else slow_fib (n - 2) + slow_fib (n - 1)

let rec fast_fib_helper n pp p = if (n = 1) then p
else fast_fib_helper (n - 1) p (pp + p)

let fast_fib n = fast_fib_helper n 0 1

let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

let divide (numerator: float) (denominator: float) = numerator /. denominator

let add x y = x + y
let associativity_1 = add 5 1
let associativity_2 = add 5
let associativity_3 = (add 5) 1
(** This is going to throw an error: [let associativity_4 = add (5 1)] *)

(** Custom infix operator *)
let (+/.) x y = (x +. y) /. 2.0
let infix_result = 5.5 +/. 3.67

let hello_world_with_newline = print_endline "Hello World!"
let hello_world_without_newline = print_string "Hello World!"