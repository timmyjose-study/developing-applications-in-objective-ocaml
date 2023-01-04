open Stack

module type Stringable = sig
  type t

  val to_str : t -> string
end

module IntItem : Stringable with type t = int = struct
  type t = int

  let to_str = string_of_int
end

module FloatItem : Stringable with type t = float = struct
  type t = float

  let to_str = string_of_float
end

module ListStack (S : Stringable) : Stack with type item = S.t = struct
  type item = S.t
  type t = item list

  let empty = []
  let is_empty s = s = []
  let push x s = x :: s
  let pop s = if is_empty s then failwith "stack underflow" else List.tl s
  let peek s = if is_empty s then failwith "stack underflow" else List.hd s

  let to_string s =
    "[" ^ (List.fold_left (fun acc e -> acc ^ "," ^ S.to_str e) "" s)  ^ "]"
end
