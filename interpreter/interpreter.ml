(* This is an OCaml editor. Enter your program here and send it
   to the toplevel using the "Eval code" button or [Ctrl-e]. *)
type exp =
  | N of int
  | B of bool
  | Id of string
  | Op of string * exp * exp
  | Let of string * exp * exp
  | If of exp * exp * exp
  | Fun of string * exp
  | App of exp * exp

type value =
  | Ival of int
  | Bval of bool
  | Oval of (value -> value -> value)
  | Fval of
      string
      * exp
      * (string * value) list (* (string * value) list is the environment *)

(* FVal IS a closure *)
let e0 = Op ("+", N 1, Op ("*", N 2, N 3))

(* 1+(2*3) *)
let e1 = App (Fun ("x", Op ("+", Id "x", N 1)), N 18)

(* (fun x -> x + 1) 18
  *)
let e2 = Let ("inc", Fun ("x", Op ("+", Id "x", N 1)), App (Id "inc", N 18))

(* let inc = fun x -> x + 1 in inc 18 *)
let top =
  [
    ("+", Oval (fun (Ival n1) (Ival n2) -> Ival (n1 + n2)));
    ("*", Oval (fun (Ival n1) (Ival n2) -> Ival (n1 * n2)));
  ]

let rec eval e env =
  match e with
  | N n -> Ival n
  | B b -> Bval b
  | Id x -> List.assoc x env
  | Op (op, e1, e2) ->
      let (Oval o) = List.assoc op env in
      o (eval e1 env) (eval e2 env)
  | Let (x, e1, e2) -> eval e2 ((x, eval e1 env) :: env)
  | If (e1, e2, e3) ->
      let (Bval b) =
        eval e1 env
        (* might give runtime error
           without type check*)
      in
      if b then eval e2 env else eval e3 env
  | Fun (x, e1) -> Fval (x, e1, env) (* creating the closure *)
  | App (e1, e2) ->
      let (Fval (y, e', env')) = eval e1 env in
      eval e' ((y, eval e2 env) :: env')

(* check App-explanation.md for an example of function application *)
