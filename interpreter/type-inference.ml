type exp =
  | Var of char (* x *)
  | Abs of char * char (* fun x -> e *)
  | App of exp * exp (* e1 e2 *)
  | Let of char * exp * exp

type simpleType =
  | TVar of int
  | TFun of simpleType * simpleType
  | TCon of string * simpleType list
