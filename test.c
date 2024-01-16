#include "common/common.h"
#include "common/generators.h"

void filter(int i1, loc x, loc x2) {
 if (x == 0) {
  WRITE_LOC(x2, 0, 0);
 } else {
  loc h = READ_LOC(x, 0);
  loc n = READ_LOC(x, 1);
  if (i1 <= h) {
   filter(i1, n, x2);
   loc x4 = READ_LOC(x2, 0);
   loc x = (loc)malloc(2 * sizeof(loc));
   WRITE_LOC(x2, 0, x);
   WRITE_LOC(x, 1, x4);
   WRITE_LOC(x, 0, h);
  } else {
   filter(i1, n, x2);
  }
 }
}

int main() {
  loc _out = malloc(sizeof(loc));
  int _x0 = 0;
  loc _x1 = 0;
  _x0 = 7;
  _x1 = _generateIntList();
  filter(_x0, _x1, _out);
  return 0;
}
