let hoge x = x + 1 [@@my_attr]

type fuga = string [@@my_attr]

let my_function x = x + 1
let () = my_function 2 |> Printf.printf "%d"
