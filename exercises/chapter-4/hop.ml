(* twice, no arguments *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

(* mystery operator *)
let ( $ ) f x = f x

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f
(* the @@ operator applies g to the input and pipes it to f *)

(* repeat *)
let rec repeat f n x = if n = 0 then x else repeat f (n - 1) (f x)

(* product *)
let product_left lst =
  List.fold_left ( *. ) 1.0 lst (* lst argument can be omitted*)

let product_right lst = List.fold_right ( *. ) lst 1.0

(*
   let rec whatever_right f xs e = match xs with
   [] -> e
   | y :: ys -> f y (whatever_right f ys e)
*)

(* terse product *)
(* already terse i guess *)

(* sum_cube_odd *)

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let ( -- ) i j = from i j []

let sum_cube_odd n =
  let l = 0 -- n in
  let odds = List.filter (fun i -> i mod 2 = 1) l in
  let odd_cubes = List.map (fun i -> i * i * i) odds in
  List.fold_left ( + ) 0 odd_cubes

(* sum_cube_odd pipeline  *)
let sum_cube_odd_pipeline n =
  0 -- n
  |> List.filter (fun i -> i mod 2 = 1)
  |> List.map (fun i -> i * i * i)
  |> List.fold_left ( + ) 0

(* exists *)
let rec exists_rec p lst =
  match lst with [] -> false | h :: t -> p h = true || exists_rec p t

let exists_fold p lst = List.fold_left (fun acc elt -> acc || p elt) false lst
(* Using acc || p elt ensures that if acc is true, the result of the expression will be true, regardless of the value of p elt. This is crucial because once we have found an element that satisfies the predicate, we can stop further evaluation and immediately return true.

   On the other hand, if we were to write p elt || acc, the short-circuiting behavior would not be preserved. In this case, the evaluation would always proceed, even if acc is true. This can lead to unnecessary computation and potentially incorrect results. *)

let exists_lib p lst = List.exists p lst

(* account balance  *)
let rec account_balance balance debits =
  match debits with
  | [] -> balance
  | debit :: rest -> account_balance (balance - debit) rest

let rec fold_left_account_balance balance debits =
  List.fold_left (fun acc debit -> acc - debit) balance debits

let rec fold_right_account_balance balance debits =
  balance - List.fold_right (fun acc debit -> acc + debit) debits 0

(* note fold_left and fold_right are not equivalent in non-associative operations *)

(* library uncurried *)

let uncurried_append (l1, l2) = List.append l1 l2
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (n1, n2) = Stdlib.max n1 n2

(* map composition *)
let map_comp f g lst = List.map f (List.map g lst)
let map_comp2 f g lst = List.map (fun elt -> f (g elt)) lst

(* more list fun *)

let greater_than_3 lst = List.filter (fun x -> String.length x > 3) lst
let add1 lst = List.map (fun x -> x +. 1.0) lst

let custom_concat lst sep =
  match lst with
  | [] -> ""
  | h :: t -> List.fold_left (fun acc str -> acc ^ sep ^ str) h t

(* in the above code we fold with tail so that the first element doesn't appear twice *)

(* association list keys *)

let rec collect_unique_keys keys_seen assoc_list =
  match assoc_list with
  | [] -> keys_seen
  | (key, _) :: rest ->
      if List.mem key keys_seen then collect_unique_keys keys_seen rest
      else collect_unique_keys (key :: keys_seen) rest

let keys assoc_list = collect_unique_keys [] assoc_list |> List.rev
let keys2 xs = xs |> List.map fst |> List.sort_uniq compare

(* valid matrix *)

let rec is_valid_matrix_helper matrix length_of_first_row =
  match matrix with
  | [] -> true (* here it should be true if not result will always be false*)
  | r :: other_rows ->
      List.length r = length_of_first_row
      && is_valid_matrix_helper other_rows length_of_first_row

let is_valid_matrix matrix =
  if matrix = [] then false
  else is_valid_matrix_helper matrix (List.hd matrix |> List.length)

(* alternative solution *)

let is_valid_matrix2 m =
  m != [] && m != [ [] ]
  && 1 = List.length (m |> List.map List.length |> List.sort_uniq compare)

(* row vector add *)
let add_row_vectors l1 l2 = List.map2 (fun a b -> a + b) l1 l2

(* matrix add *)
let add_matrices m1 m2 = List.map2 (fun r1 r2 -> add_row_vectors r1 r2) m1 m2

(* matrix multiply *)

let rec transpose mat =
  match mat with
  | [] -> []
  | [] :: _ -> [] (* we have reached the end of columns if first row is empty*)
  | _ -> List.map List.hd mat :: transpose (List.map List.tl mat)

let dot_product v1 v2 = List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2

let multiply_matrices m1 m2 =
  let transposed_m2 = transpose m2 in
  let multiply_row row = List.map (dot_product row) transposed_m2 in
  List.map multiply_row m1
