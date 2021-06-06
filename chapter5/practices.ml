module type ComplexSig = sig
  type t = float * float
  
  val zero : t
  val add : t -> t -> t
end

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module FractionImpl: Fraction = struct
  type t = int * int

  let make (n: int) (d: int) = if d = 0 then
    failwith "The denominator cannot be zero."
  else (n, d)

  let numerator (n, d) = n
  
  let denominator (n, d) = d

  let to_string (n, d) = Printf.sprintf "(%d, %d)" n d

  let to_float (n, d) = float_of_int(n) /. float_of_int(d)

  let add a b =
    let (x, y) = a in
    let (x', y') = b in
    (x + x', y + y')

  let mul a b =
    let (x, y) = a in
    let (x', y') = b in
    (x * x', y * y')
end

module CharMap = Map.Make(Char)

let _ =
  let char_map = CharMap.empty
  |> CharMap.add 'A' "Alpha"
  |> CharMap.add 'E' "Echo"
  |> CharMap.add 'S' "Sierra"
  |> CharMap.add 'V' "Victor" in
  Printf.printf "E is %s.\n" (CharMap.find 'E' char_map);
  let char_map = CharMap.remove 'A' char_map in
  Printf.printf "Is A still there? %s.\n" (Bool.to_string (CharMap.mem 'A' char_map));
  List.iter (fun (k, v) -> Printf.printf "%c: %s\n" k v) (CharMap.bindings char_map)

module type ToString = sig
  type t
  
  val to_string : t -> string
end

module Print = functor (M: ToString) -> struct
  let print value = print_endline (M.to_string value)
end

module Int: ToString with type t = int = struct
  type t = int

  let to_string value = string_of_int value
end

module PrintInt = Print(Int)

let _ = PrintInt.print 123

module MyString: ToString with type t = string = struct
  type t = string

  let to_string (value: t) = value
end

module PrintString = Print(MyString)

let _ = PrintString.print "Hello"

module StringWithPrint = struct
  include String
  include Print(MyString)
end

let _ = StringWithPrint.print "Hi"