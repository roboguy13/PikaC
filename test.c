#include "common/common.h"
#include "common/generators.h"

void treeSize(loc x, loc i1) {
 if (x == 0) {
  WRITE_LOC(i1, 0, 1);
 } else {
  loc p = READ_LOC(x, 1);
  loc q = READ_LOC(x, 2);
  treeSize(p, i1);
  loc i3 = READ_LOC(i1, 0);
  treeSize(q, i1);
  loc i = READ_LOC(i1, 0);
  WRITE_LOC(i1, 0, (long)((long)1 + (long)i3) + (long)i);
 }
}

int main() {
  loc _out = malloc(sizeof(loc));
  loc _x0 = 0;
  _x0 = _generateBinaryTree();
  treeSize(_x0, _out);
  loc _derefOut = READ_LOC(_out, 0);
  _printInt(_derefOut);

  return 0;
}
