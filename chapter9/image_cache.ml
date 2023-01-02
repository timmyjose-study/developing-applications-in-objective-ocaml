type table_of_images = {
  size : int;
  mutable ind : int;
  mutable name : string;
  mutable current : Graphics.color array array;
  cache : (string * Graphics.color array array) Weak.t;
}

let open_image filename =
  let ic = open_in filename in
  let i = (input_value ic : Graphics.color array array) in
  close_in ic;
  i
