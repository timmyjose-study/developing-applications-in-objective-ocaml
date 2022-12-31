type 'a stack = { mutable idx : int; size : int; mutable arr : 'a array }

exception Stack_empty
exception Stack_full

let init_stack sz = { idx = -1; size = sz; arr = [||] }

let pop st =
  if st.idx = -1 then raise Stack_empty
  else
    let ret = st.arr.(st.idx) in
    st.idx <- st.idx - 1;
    ret

let peek st = if st.idx = -1 then raise Stack_empty else st.arr.(st.idx)

let push e st =
  if st.arr = [||] then (
    st.arr <- Array.make st.size e;
    st.idx <- 0)
  else if st.idx >= st.size - 1 then raise Stack_full
  else (
    st.idx <- st.idx + 1;
    st.arr.(st.idx) <- e)
