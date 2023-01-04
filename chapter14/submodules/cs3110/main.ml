module type MY_MODULE_SIG = sig
  val inc : int -> int

  type primary_color = Red | Green | Blue

  exception Oops
end

module MyModule : MY_MODULE_SIG = struct
  let inc x = x + 1

  type primary_color = Red | Green | Blue

  exception Oops
end