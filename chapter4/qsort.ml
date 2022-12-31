Random.self_init ()

let permute_element vec i j =
  let t = vec.(i) in
  vec.(i) <- vec.(j);
  vec.(j) <- t

let choose_pivot vec low high = low

let permute_pivot vec low high pidx =
  permute_element vec low pidx;
  let i = ref (low + 1) and j = ref high and pivot = vec.(low) in
  while !i < !j do
    if vec.(!j) >= pivot then decr j
    else (
      permute_element vec !i !j;
      incr i)
  done;
  if vec.(!i) > pivot then decr i;
  permute_element vec low !i;
  !i

let rec quick vec low high =
  if low < high then
    let pivot = choose_pivot vec low high in
    let place_pivot = permute_pivot vec low high pivot in
    quick (quick vec low (place_pivot - 1)) (place_pivot + 1) high
  else vec

let quick_sort vec = quick vec 0 (Array.length vec - 1)

let get_random_array size low high =
  let r = Array.make size 0 in
  for i = 0 to size - 1 do
    r.(i) <- Random.int (high - low) + low
  done;
  r