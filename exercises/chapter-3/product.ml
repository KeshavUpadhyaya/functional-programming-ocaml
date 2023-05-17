let rec product lst = match lst with [] -> 1 | h :: t -> h * product t
