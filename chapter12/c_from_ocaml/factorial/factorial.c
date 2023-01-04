#include <caml/mlvalues.h>
#include <stdio.h>

value factorial(value n) {
  long m = Long_val(n);
  long f = 1L;

  for (int i = 1; i <= m; i++) {
    f *= i;
  }

  return Val_long(f);
}