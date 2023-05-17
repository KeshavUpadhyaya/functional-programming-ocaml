type intlist = Nil | Cons of int * intlist

let rec length = function Nil -> 0 | Cons (_, t) -> 1 + length t

type 'a mylist = Nil | ( :: ) of 'a * 'a mylist

let rec length = function Nil -> 0 | _ :: t -> 1 + length t
let example : int mylist = 1 :: 2 :: Nil
