(* take drop tail recursive *)

let rec take_helper n lst acc =
  if n = 0 then acc
  else match lst with [] -> acc | h :: t -> take_helper (n - 1) t (h :: acc)
(*acc @ [ h ] is more time consuming*)

let take n lst = take_helper n lst [] |> List.rev

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | h :: t -> drop (n - 1) t
(* this function is already tail recursive*)

(* unimodal *)

let rec is_decreasing lst =
  match lst with
  | [] | [ _ ] -> true
  | a :: b :: cs -> b <= a && is_decreasing (b :: cs)

let rec is_increasing_then_decreasing lst =
  match lst with
  | [] | [ _ ] -> true
  | a :: b :: cs ->
      if b >= a then is_increasing_then_decreasing (b :: cs)
      else is_decreasing (a :: b :: cs)

let rec is_unimodal lst = is_increasing_then_decreasing lst

(* safe hd and tl *)

let safe_hd lst = match lst with [] -> None | h :: _ -> Some h
let safe_tl lst = match lst with [] -> None | _ :: t -> Some t

(*
if you just say h :: _-> h in the safe_hd it just restricts the type but works!
   *)

(*quadrant*)

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x = 0 then Zero else if x < 0 then Neg else Pos

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _, Zero | Zero, _ -> None

(*  depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth root =
  match root with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

(* shape *)
let rec same_shape (t1, t2) =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) ->
      same_shape (l1, l2) && same_shape (r1, r2)
  | _ -> false

(*is bst*)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* Assuming that the tree contains only integers *)

let rec find_max_value tree =
  match tree with
  | Leaf -> min_int
  | Node (value, left, right) ->
      let max_left = find_max_value left in
      let max_right = find_max_value right in
      max value (max max_left max_right)

let rec find_min_value tree =
  match tree with
  | Leaf -> max_int
  | Node (value, left, right) ->
      let min_left = find_min_value left in
      let min_right = find_min_value right in
      min value (min min_left min_right)

let rec is_bst tree =
  match tree with
  | Leaf -> true
  | Node (value, left, right) ->
      find_max_value left < value
      && find_min_value right > value
      && is_bst left && is_bst right

let t1 = Node (3, Node (1, Leaf, Leaf), Node (4, Leaf, Leaf))
let t2 = Node (3, Node (10, Leaf, Leaf), Node (1, Leaf, Leaf))
(* let rec is_bst_helper tree min_value max_value =
     match tree with
     | Leaf -> true
     | Node (value, left, right) ->
         value > min_value && value < max_value
         && is_bst_helper left min_value value
         && is_bst_helper right value max_value

   let rec is_bst tree = is_bst_helper tree min_int max_int *)

(*alternative using inorder traversal *)

let rec inorder t =
  match t with Leaf -> [] | Node (x, l, r) -> inorder l @ [ x ] @ inorder r

let rec is_asc xs =
  match xs with x :: y :: xs -> x <= y && is_asc (y :: xs) | _ -> true

(* let is_bst tree = inorder tree |> is_asc *)
