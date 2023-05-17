let get_val o = match o with None -> failwith "??" | Some x -> x

let rec list_max lst =
  match lst with
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

let x = list_max [ 1; 2; 3 ]
let y = list_max []
let extract o = match o with Some i -> string_of_int i | None -> ""
