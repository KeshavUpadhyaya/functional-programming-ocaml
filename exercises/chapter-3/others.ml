(*concat*)

let rec concat lst = match lst with [] -> "" | h :: t -> h ^ concat t

(*patterns*)

let is_first_element_bigred lst =
  match lst with [] -> false | h :: t -> h = "bigred"

let rec my_list_length lst =
  match lst with [] -> 0 | h :: t -> 1 + my_list_length t

let does_list_have_exactly_2_or_4_elements lst =
  my_list_length lst = 2 || my_list_length lst = 4

let are_first_two_elements_equal lst =
  match lst with [] -> false | a :: b :: _ -> a = b | _ -> false

(* Library *)
let fifth_element lst = if List.length lst >= 5 then List.nth lst 4 else 0
let sorted_desc lst = List.sort Stdlib.compare lst |> List.rev

(* take drop *)
let rec take n lst =
  if n = 0 then []
  else match lst with [] -> [] | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | h :: t -> drop (n - 1) t

(* powerset *)

let rec powerset lst =
  match lst with
  | [] -> [ [] ]
  | h :: t ->
      let ps = powerset t in
      List.map (fun ls -> h :: ls) ps @ ps

(* student *)
type student = { first_name : string; last_name : string; gpa : float }

let s1 : student = { first_name = "a"; last_name = "b"; gpa = 3.4 }
let student_name student = (student.first_name, student.last_name)
let student_record first_name last_name gpa = { first_name; last_name; gpa }

(* pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard : pokemon = { name = "charizard"; hp = 78; ptype = Fire }

(* date before *)
type date = int * int * int

let is_before (date1 : date) (date2 : date) =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(* matching *)
let l1 : int option list = [ None; Some 0 ]
let l2 = [ Some 4110; None ]
let l3 = [ None; Some 23 ]
let l4 = [ Some 0; None ]
(* h :: tl matches any non empty list so its impossible to come up with an example *)
