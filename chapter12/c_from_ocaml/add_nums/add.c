// Use `ocamlc -where` to locate the header file.
#include <caml/mlvalues.h>
#include <stdio.h>

value plus_native(value x1, value x2, value x3, value x4, value x5, value x6) {
  printf("<<NATIVE PLUS>>\n");

  return Val_long(Long_val(x1) + Long_val(x2) + Long_val(x3) + Long_val(x4) +
                  Long_val(x5) + Long_val(x6));
}

value plus_bytecode(value *val_arr, int num_val) {
  printf("<<BYTECODE PLUS>>\n");

  long res = 0L;
  for (int i = 0; i < num_val; i++) {
    res += Val_long(val_arr[i]);
  }

  return res;
}