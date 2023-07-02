type 'a pointer = 'a ref option

let null : 'a pointer = None
let malloc x = Some (ref x)
let p = malloc 42

exception Segfault

let deref (ptr : 'a pointer) : 'a =
  match ptr with None -> raise Segfault | Some r -> !r

let ( ~* ) = deref
let assign ptr x = match ptr with None -> raise Segfault | Some r -> r := x
let fact0 = ref (fun x -> x + 0)
let fact n = if n = 0 then 1 else n * !fact0 (n - 1)

type 'a node = { next : 'a mlist; value : 'a }
and 'a mlist = 'a node option ref

let empty () = ref None
let insert_first lst v = lst := Some { next = ref !lst; value = v }

let rec to_list lst =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next
