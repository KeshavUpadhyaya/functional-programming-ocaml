(* mutable fields *)
type student = { name : string; mutable gpa : float }

let s1 = { name = "Alice"; gpa = 3.7 }
let a = s1.gpa <- 3.4

(* refs *)
let a = ref true
let b = ref [ 1 ]
let c = [ ref 1 ]

(* inc fun *)
let inc = ref (fun x -> x + 1)
let d = !inc 3109

(* addition assignment *)
let ( +:= ) x y = x := !x + y

(* physical equality *)
let x = ref 0
let y = x
let z = ref 0

(* # x == y;; true*)
(* # x == z;; false*)
(* # x = y;; true*)
(* # x = z;; true*)
(* # x := 1;; unit*)
(* # x = y;; true*)
(* # x = z;; false*)

(* norm *)
(* AF: the float array [| x1; ...; xn |] represents the
 *     vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

let norm (v : vector) =
  Array.map (fun x -> x *. x) v |> Array.fold_left ( +. ) 0. |> sqrt

(* normalize *)
let normalize (v : vector) : unit =
  let n = norm v in
  Array.iteri (fun i x -> v.(i) <- x /. n) v

(* norm loop *)

let norm2 (v : vector) =
  let n = ref 0.0 in
  for i = 0 to Array.length v - 1 do
    n := !n +. (v.(i) ** 2.)
  done;
  sqrt !n

(* normalize loop *)
let normalize2 v =
  let n = norm v in
  for i = 0 to Array.length v - 1 do
    v.(i) <- v.(i) /. n
  done

(* init matrix *)
let init_matrix n o f = Array.init n (fun i -> Array.init o (fun j -> f i j))

(* while loop demo!! *)
let gcdi a1 b1 =
  let a = ref a1 in
  let b = ref b1 in
  while not (!a = !b) do
    if !a < !b then b := !b - !a else a := !a - !b
  done;
  a

let rec gcd a b =
  if a = b then a else if a < b then gcd a (b - a) else gcd (a - b) b
