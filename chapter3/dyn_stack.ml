type 'a stack = {
  mutable idx : int;
  mutable size : int;
  mutable arr : 'a array;
}

exception Stack_empty

let size st = st.idx + 1
let capacity st = Array.length st.arr
let peek st = if st.idx = -1 then raise Stack_empty else st.arr.(st.idx)

let shrink_stack st =
  let new_size = st.size / 2 in
  let new_arr = Array.make new_size (peek st) in
  for i = 0 to st.idx do
    new_arr.(i) <- st.arr.(i)
  done;
  st.size <- new_size;
  st.arr <- new_arr

let pop st =
  if st.arr = [||] then raise Stack_empty
  else if st.idx < st.size / 2 then (
    let ret = st.arr.(st.idx) in
    shrink_stack st;
    st.idx <- st.idx - 1;
    ret)
  else
    let ret = st.arr.(st.idx) in
    st.idx <- st.idx - 1;
    ret

let expand_stack st =
  let new_size = 2 * st.size in
  let new_arr = Array.make new_size (peek st) in
  for i = 0 to st.idx do
    new_arr.(i) <- st.arr.(i)
  done;
  st.size <- new_size;
  st.arr <- new_arr

let init_stack sz = { idx = -1; size = sz; arr = [||] }

let push e st =
  if st.arr = [||] then (
    st.arr <- Array.make st.size e;
    st.idx <- 0)
  else if st.idx = st.size - 1 then (
    expand_stack st;
    st.idx <- st.idx + 1;
    st.arr.(st.idx) <- e)
  else (
    st.idx <- st.idx + 1;
    st.arr.(st.idx) <- e)