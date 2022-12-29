type complex =  { re : float ; im : float }

let c1 = { re = 1.23 ; im = -0.002 }
let c2 = { re = -1.23 ; im = +0.002 }
let c3 = { c2 with im = 123.45 }

let add_complex c1 c2 =
  let { re = re1 ; im = im1 } = c1 and
  {re = re2; im = im2 } = c2 in
  { re = re1 +. re2 ; im = im1 +. im2 }

let mult_complex c1 c2 = 
  let { re = re1 ; im = im1 } = c1 and
  { re = re2 ; im = im2 } = c2 in
  { re = re1 *. re2 -. im1 *. im2 ; im = re1 *. im2 +. re2 *. im1 }
