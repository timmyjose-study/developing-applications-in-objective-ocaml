let get_random low high = Random.int (high - low) + low

let rec play secret =
  print_string "What is your guess? ";
  let guess = read_int () in
  if guess < secret then (
    print_endline "Too low! Try again...";
    play secret)
  else if guess > secret then (
    print_endline "Too high! Try again...";
    play secret)
  else print_endline "Correct!"

let game () =
  let secret = get_random 1 100 in
  play secret