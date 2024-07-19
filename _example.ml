let foo x =
  (* This is a comment *)
  x + 1
[@@my_attr] [@@other_attr]

let fake x = x + 1 [@@fake_attr]

type fuga = string [@@my_attr]

let piyo = "piyo" [@@my_attr]
let my_function x = x + 1
let () = my_function 2 |> Printf.printf "%d"
