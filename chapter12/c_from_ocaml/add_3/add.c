#include <caml/mlvalues.h>
#include <stdio.h>

value plus_bytecode(value x1, value x2, value x3) {
  printf("<<BYTECODE PLUS>>\n");
  return Val_long(Long_val(x1) + Long_val(x2) + Long_val(x3));
}

// value plus_bytecode(value *arr_val, int num_val) {
//  long res = 0L;
//
//  for (int i = 0; i < num_val; i++) {
//    res += Long_val(arr_val[i]);
//  }
//
//  return Val_long(res);
//}

value plus_native(value x1, value x2, value x3) {
  printf("<<NATIVE PLUS>>\n");
  return Val_long(Long_val(x1) + Long_val(x2) + Long_val(x3));
}
