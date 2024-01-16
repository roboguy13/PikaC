#include "common/common.h"
#include "common/generators.h"

void take(loc x1, loc x, loc x2) {
 if (x == 0) {
  if (x1 == 0) {
   WRITE_LOC(x2, 0, 0);
  } else {
   WRITE_LOC(x2, 0, 0);
  }
 } else {
  loc h = READ_LOC(x, 0);
  loc nx = READ_LOC(x, 1);
  if (x1 == 0) {
   WRITE_LOC(x2, 0, 0);
  } else {
   loc n = READ_LOC(x1, 0);
   take(n, nx, x2);
   loc x4 = READ_LOC(x2, 0);
   loc x = (loc)malloc(2 * sizeof(loc));
   WRITE_LOC(x2, 0, x);
   WRITE_LOC(x, 1, x4);
   WRITE_LOC(x, 0, h);
  }
 }
}

int main() {
  loc _out = malloc(sizeof(loc));
  loc _x0 = 0;
  loc _x1 = 0;
  _x0 = _generateNat();
  _x1 = _generateBinaryTree();
  for (int i = 0; i < TEST_ITERATIONS; ++i) {
  take(_x0, _x1, _out);
    loc _derefOut = READ_LOC(_out, 0);
  _printIntList(_derefOut);

    printf("\n");
  }
  return 0;
}
