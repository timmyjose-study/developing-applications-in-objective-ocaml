module IntMap = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

module StrSet = Set.Make (String)

(* a functor that can print items of any module that supports the `to_string` function *)

module type Printable = sig
  type t

  val to_str : t -> string
end

module Int : Printable with type t = int = struct
  type t = int

  let to_str = string_of_int
end

module Int2 = struct
  type t = int

  let to_str = string_of_int
end

module Float : Printable with type t = float = struct
  type t = float

  let to_str = string_of_float
end

module String2 : Printable with type t = string = struct
  include String

  type t = string

  let to_str s = s
end

module Printer (P : Printable) = struct
  let print x = Printf.printf "%s\n" (P.to_str x)
end

module IntPrinter = Printer (Int)
module Int2Printer = Printer (Int2)
module FloatPrinter = Printer (Float)
module String2Printer = Printer (String2)

module AnotherIntPrinter = Printer (struct
  type t = int

  let to_str = string_of_int
end)