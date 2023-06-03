let p x =
  print_int x;
  print_newline ();
  x + 1

let lst = List.map p [ 1; 2 ]
