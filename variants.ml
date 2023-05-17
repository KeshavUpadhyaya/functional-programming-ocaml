type primary_color = Red | Green | Blue

let r = Red

type point = float * float

type shape =
  | Circle of { center : point; radius : float }
  | Rectangle of { lower_left : point; upper_right : point }
  | Point of point

let c1 = Circle { center = (0., 0.); radius = 1.0 }
let r1 = Rectangle { lower_left = (-1., -1.); upper_right = (1., 1.) }
let avg a b = (a +. b) /. 2.0
let point p1 = Point (31., 10.)

let center (s : shape) =
  match s with
  | Circle { center; radius } -> center
  | Rectangle { lower_left = x_ll, y_ll; upper_right = x_ur, y_ur } ->
      (avg x_ll x_ur, avg y_ll y_ur)
  | Point (x, y) -> (x, y)

type string_or_int = String of string | Int of int
type string_or_int_list = string_or_int list

let rec sum : string_or_int list -> int = function
  | [] -> 0
  | String s :: t -> int_of_string s + sum t
  | Int i :: t -> i + sum t

let lst_sum = sum [ String "1"; Int 2 ]
