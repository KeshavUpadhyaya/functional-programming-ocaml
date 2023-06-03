type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))
let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let rec sum = function Leaf -> 0 | Node (v, l, r) -> v + sum l + sum r

type 'a tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let t =
  Node
    {
      value = 2;
      left = Node { value = 1; left = Leaf; right = Leaf };
      right = Node { value = 3; left = Leaf; right = Leaf };
    }

let rec mem x = function
  | Leaf -> false
  | Node { value; left; right } -> value = x || mem x left || mem x right

let rec preorder = function
  | Leaf -> []
  | Node { value; left; right } -> [ value ] @ preorder left @ preorder right

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map f l, map f r)

let add1 t = map succ t

let rec fold acc f = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold acc f l) (fold acc f r)

let sum t = fold 0 (fun x y z -> z + y + z) t
