let foo x =
  (* This is a comment *)
  x + 1
[@@snippet bar]

let fake x = x + 1 [@@fake_attr]

type fuga = string [@@snippet]

let piyo = "piyo" [@@snippet]
let my_function x = x + 1
let () = my_function 2 |> Printf.printf "%d"
