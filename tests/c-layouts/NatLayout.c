#include "NatLayout.h"

int main() {
  loc x = malloc(sizeof(loc));
  generate_NatLayout(7, x);
  print_NatLayout(x);
  printf("\n");
}
