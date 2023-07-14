type aexp =
  | C of int
  | Sum of aexp * aexp
  | Prod of aexp * aexp
  | Diff of aexp * aexp
  | Quot of aexp * aexp
  | Var of char
  | Let of char * aexp * aexp

let ae1 = Sum (C 1, C 2) (* 1 + 2 *)
let ae2 = Prod (ae1, ae1) (* (1 + 2) * (1 + 2) *)
let ae3 = Let ('x', C 7, Prod (Var 'x', ae1)) (* let x = 7 in x * (1 + 2) *)

(* Environment: a set of bindings.
   Binding: pair of a name/var and a value.
   So far: (char * int) list *)
let env : (char * int) list ref = ref []

(* evalA: aexp -> int
   env: (char * int) list *)
let rec evalA ae =
  match ae with
  | C n -> n
  | Sum (a1, a2) -> evalA a1 + evalA a2
  | Prod (a1, a2) -> evalA a1 * evalA a2
  | Diff (a1, a2) -> evalA a1 - evalA a2
  | Quot (a1, a2) -> evalA a1 / evalA a2
  | Var x -> List.assoc x !env
  | Let (x, e1, e2) ->
      env := (x, evalA e1) :: !env;
      evalA e2
