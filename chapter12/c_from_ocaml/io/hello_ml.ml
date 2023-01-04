external hello_world : unit -> unit = "hello_world";;

print_string "<< ";
flush stdout;
hello_world ();
print_string " >>\n";
flush stdout;
print_newline ()
