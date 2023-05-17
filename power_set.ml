(* Compute it once so "let ps = powerset" *)

(* add elem to list of lists*)
let rec add1 elem xs =
  match xs with [] -> [] | h :: t -> (elem :: h) :: add1 elem t

let rec powerset xs =
  match xs with
  | [] -> [ [] ]
  | h :: t ->
      let ps = powerset t in
      ps @ add1 h ps
