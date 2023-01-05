class colored_point (x, y) c =
  object
    inherit point (x, y)
    val mutable c = c
    method get_color = c
    method set_color nc = c <- nc

    method to_string () =
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ c ^ ")"
  end