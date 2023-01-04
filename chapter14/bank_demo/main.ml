open Log
open Account

let main () =
  let mgr = Mbank.create 100. 10. and cust = Mcustomer.create 100. 10. in
  Mbank.deposit 20. mgr;
  Mbank.withdraw 30. mgr;
  Printf.printf "mgr balance = %.2f\n" (Mbank.balance mgr);
  Mcustomer.deposit 12345. cust;
  Printf.printf "cust balance = %.2f\n" (Mcustomer.balance cust);
;;

main ()
