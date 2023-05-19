#include "common/common.h"
#include "Sll.h"

void mapAdd1(loc x_, loc x1_) {
 loc x = READ_LOC(x_, 0);

 if (x == 0) {
   WRITE_LOC(x1_, 0, 0);
  /* WRITE_LOC(x1, 1, 0); */
 } else {
  if (! (x == 0)) {
   loc x1 = malloc(2 * sizeof(loc));
   /* x1 = (loc)malloc(2 * sizeof(loc)); */

   int h2 = READ_INT(x, 0);
   loc t4 = READ_LOC(x, 1);

   loc zz3_ = malloc(sizeof(loc));
   mapAdd1(&t4, zz3_);
   loc zz3 = READ_LOC(zz3_, 0);

   WRITE_INT(x1, 0, h2 + 1);
   WRITE_LOC(x1, 1, zz3);

   WRITE_LOC(x1_, 0, x1);
  } else {
  }
 }

}


int main() {
  loc x = malloc(sizeof(loc));
  generateRange_Sll(0, 10, x);

  loc y = malloc(sizeof(loc));

  loc list = READ_LOC(x, 0);
  mapAdd1(x, y);

  print_Sll(y);
  printf("\n");
  return 0;
}

