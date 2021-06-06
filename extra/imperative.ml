let student name = object
  val mutable student_name = name
  
  method say_hello = Printf.printf "Hi, my name is %s. Nice to meet you!" student_name
  method private get_score = 123
end

let john = student "John"
let _ = john#say_hello

class virtual creature = object
  method virtual eat: unit -> unit
end

class virtual animal name = object
  inherit creature

  val mutable animal_name = name
  method virtual yell: unit -> unit
end

let cat name = object
  inherit animal name

  method yell () = print_endline "Meow!"
  
  method eat () = print_endline "Hooman, give me food already!"
end

let tracy = cat "Tracy"