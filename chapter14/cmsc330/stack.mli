module type Stack = sig
  type item
  type t

  val empty : t
  val is_empty : t -> bool
  val push : item -> t -> t
  val pop : t -> t
  val peek : t -> item
  val to_string : t -> string
end