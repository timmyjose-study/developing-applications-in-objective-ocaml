#include <caml/mlvalues.h>
#include <stdio.h>

value hello_world(value v) {
  printf("Hello, world!");
  fflush(stdout);

  return v;
}