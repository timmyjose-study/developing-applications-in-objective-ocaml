let imply v = 
  match v with
  (true, true) -> true
  | (true, false) -> false
  | (false, true) -> true
  | (false, false) -> true

let imply v = 
  match v with
  (true, false) -> false
  | _ -> true

let is_zero n = 
  match n with
  0 -> true
  | _ -> false

let is_a_vowel c = 
  match c with
  'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

type rational = { numerator : int ; denominator : int }

(* note that the parentheses is mandatory when using `as`:

   (p as name))
*)
let min_rat pr = 
  match pr with
   ({ numerator = _ ; denominator = 0 } , ({ numerator ; denominator } as r2)) -> r2
  | (({numerator ; denominator } as r1), { numerator =  _ ; denominator = 0 } ) -> r1
  | (({ numerator = num1 ; denominator = denom1 } as r1), ({ numerator = num2 ; denominator = denom2 } as r2)) -> 
      if num1 * denom2 < num2 * denom1 then r1 else r2

let eq_rat pr = 
  match pr with
  ({ numerator = _ ; denominator = 0 }, { numerator = _ ; denominator = 0 }) -> true
  | ({ numerator ; denominator = 0 }, _) -> false
  | (_ ,  {numerator ; denominator = 0}) -> false
  | ({ numerator = num1 ; denominator = denom1}, { numerator = num2 ; denominator = denom2 }) when num1 = num2 -> true
  | ({ numerator = num1 ; denominator = denom1}, { numerator = num2 ; denominator = denom2 }) when num1 * denom2 = num2 * denom1 -> true
  | _ -> false

let char_discriminate c = 
  match c with
  'a' | 'A' | 'e' | 'E' | 'i' | 'I' | 'o' | 'O'| 'u' | 'U'-> "Vowel"
  | 'a'..'z' | 'A' .. 'Z' -> "Consonant"
  | '0' .. '9' -> "Digit"
  | _ -> "Other"

let rec size = function
  [] -> 0
  | _ :: t -> 1 + size t

let rec fold_left f acc = function
  [] -> acc
  | h :: t -> fold_left f (f acc h) t

let sum = fold_left ( + ) 0 
let prod = fold_left ( * ) 1

let range from till = 
  let rec range_aux from till acc = 
    if till < from then acc
    else range_aux from (till - 1) (till  :: acc)
  in
  range_aux from till []

