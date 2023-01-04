external plus : int -> int -> int -> int = "plus_bytecode" "plus_native";;

print_int (plus 1 2 3);
print_newline ()