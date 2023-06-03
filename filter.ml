let rec evens = function
  | [] -> []
  | h :: t -> if h mod 2 = 0 then h :: evens t else evens t

let rec odds = function
  | [] -> []
  | h :: t -> if not (h mod 2 = 0) then h :: odds t else odds t

let rec filter_aux p acc = function
  | [] -> List.rev acc
  | h :: t -> filter_aux p (if p h then h :: acc else acc) t

let rec filter p lst = filter_aux p [] lst
let even x = x mod 2 = 0
let odd x = x mod 2 = 1
let evens' lst = filter_aux even lst
let odds' lst = filter_aux odd lst
