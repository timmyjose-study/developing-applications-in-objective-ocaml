(* immutable lists - uses structural sharing *)

type 'a list_immutable = LInil | LIcons of 'a * 'a list_immutable

let li1 = LIcons (1, LIcons (2, LInil))
let li2 = LIcons (3, LIcons (4, LIcons (5, LInil)))

let rec concat l1 l2 =
  match l1 with LInil -> l2 | LIcons (h, t) -> LIcons (h, concat t l2)

let rec tlLI = function
  | LInil -> failwith "tlLI : empty list"
  | LIcons (_, t) -> t

let li3 = concat li1 li2
let () = assert (tlLI (tlLI li3) == li2)

(* mutable lists - we can decide to share sttructure or do shallow copying *)
type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let rec concat_copy l1 l2 =
  match l1 with
  | LMnil -> l2
  | LMcons (h, t) -> LMcons (h, ref (concat_copy !t l2))

let rec tlLM = function
  | LMnil -> failwith "tlLM : empty list"
  | LMcons (_, t) -> !t

let lm1 = LMcons (1, ref (LMcons (2, ref LMnil)))
let lm2 = LMcons (3, ref (LMcons (4, ref (LMcons (5, ref LMnil)))))
let lm3 = concat_copy lm1 lm2
let () = assert (tlLM (tlLM lm3) == lm2)

let concat_share l1 l2 =
  match l1 with
  | LMnil -> l2
  | _ ->
      let rec set_last = function
        | LMnil -> failwith "concat_share : impossible case"
        | LMcons (_, l) -> if !l = LMnil then l := l2 else set_last !l
      in
      set_last l1;
      l1

let lm4 = concat_share lm1 lm2
let () = assert (tlLM (tlLM lm4) == lm2) (* lm1 has been modified now! *)