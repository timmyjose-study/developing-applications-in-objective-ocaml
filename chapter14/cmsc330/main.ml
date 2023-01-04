open List_stack
module IntStack = ListStack (IntItem)

let s1 = IntStack.(empty |> push 1 |> push 2 |> push 3 |> push 4 |> push 5)
let () = Printf.printf "%s\n" (IntStack.to_string s1)

module FloatStack = ListStack (FloatItem)

let s2 = FloatStack.(empty |> push 3.14159 |> push 2.782128)
let () = Printf.printf "%s\n" (FloatStack.to_string s2)
