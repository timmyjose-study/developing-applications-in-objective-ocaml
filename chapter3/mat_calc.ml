type mat = { n : int; m : int; arr : float array array }

let create_mat n m = { n; m; arr = Array.make_matrix n m 0.0 }
let access_mat m i j = m.arr.(i).(j)
let mod_mat m i j e = m.arr.(i).(j) <- e

let add_mat p q =
  assert (p.n = q.n && p.m = q.m);
  let nrows = p.n and ncols = p.m in
  let r = create_mat nrows ncols in
  for i = 0 to nrows - 1 do
    for j = 0 to ncols - 1 do
      mod_mat r i j (access_mat p i j +. access_mat q i j)
    done
  done;
  r

let mult_mat p q =
  if p.m = q.n then (
    let r = create_mat p.n q.m in
    for i = 0 to p.n - 1 do
      for j = 0 to q.m - 1 do
        let c = ref 0.0 in
        for k = 0 to p.m - 1 do
          c := !c +. (p.arr.(i).(k) *. q.arr.(k).(j))
        done;
        mod_mat r i j !c
      done
    done;
    r)
  else failwith "mul_mat : incompatible dimensions"