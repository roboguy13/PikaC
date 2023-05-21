#include "common/common.h"
#include "Sll.h"

/* void mapAdd1(loc x, loc x1_) { */
/* #<{(|  loc x = READ_LOC(x_, 0); |)}># */
/*  if (x == 0) { */
/*    WRITE_LOC(x1_, 0, 0); */
/*   #<{(| WRITE_LOC(x1, 1, 0); |)}># */
/*  } else { */
/*   if (! (x == 0)) { */
/*    loc x1 = malloc(2 * sizeof(loc)); */
/*    #<{(| x1 = (loc)malloc(2 * sizeof(loc)); |)}># */
/*    int h2 = READ_INT(x, 0); */
/*    loc t4 = READ_LOC(x, 1); */
/*    loc zz3_ = malloc(sizeof(loc)); */
/*    mapAdd1(t4, zz3_); */
/*    loc zz3 = READ_LOC(zz3_, 0); */
/*    WRITE_INT(x1, 0, h2 + 1); */
/*    WRITE_LOC(x1, 1, zz3); */
/*    WRITE_LOC(x1_, 0, x1); */
/*   } else { */
/*   } */
/*  } */
/* } */

/* void mapAdd1(loc x, loc x1) { */
/*  if (x == 0) { */
/*   WRITE_LOC(x1, 0, 0); */
/*  } else { */
/*   if (! (x == 0)) { */
/*    loc h2 = READ_LOC(x, 0); */
/*    loc t5 = READ_LOC(x, 1); */
/*    loc x4; */
/*    x4 = (loc)malloc(2 * sizeof(loc)); */
/*    loc zz3_ = malloc(sizeof(loc)); */
/*    mapAdd1(t5, zz3_); */
/*    loc zz3 = READ_LOC(zz3_, 0); */
/*    WRITE_LOC(x4, 0, h2 + 1); */
/*    WRITE_LOC(x4, 1, zz3); */
/*    WRITE_LOC(x1, 0, x4); */
/*   } else { */
/*   } */
/*  } */
/* } */

/* void mapAdd1(loc x, loc x1) { */
/*  if (x == 0) { */
/*   WRITE_LOC(x1, 0, 0); */
/*  } else { */
/*   if (! (x == 0)) { */
/*    loc h2 = READ_LOC(x, 0); */
/*    loc t5 = READ_LOC(x, 1); */
/*    loc x4 = (loc)malloc(2 * sizeof(loc)); */
/*    loc zz6 = (loc)malloc(1 * sizeof(loc)); */
/*    mapAdd1(t5, zz6); */
/*    loc zz3 = READ_LOC(zz6, 0); */
/*    WRITE_LOC(x4, 0, (int)h2 + 1); */
/*    WRITE_LOC(x4, 1, zz3); */
/*    WRITE_LOC(x1, 0, x4); */
/*   } else { */
/*   } */
/*  } */
/* } */

/* void mapAdd1(loc x, loc x1) { */
/*  if (x == 0) { */
/*   WRITE_LOC(x1, 0, 0); */
/*  } else { */
/*   if (! (x == 0)) { */
/*    loc h2 = READ_LOC(x, 0); */
/*    loc t5 = READ_LOC(x, 1); */
/*    loc x4 = (loc)malloc(2 * sizeof(loc)); */
/*    loc zz6 = (loc)malloc(1 * sizeof(loc)); */
/*    mapAdd1(t5, zz6); */
/*    loc zz3 = READ_LOC(zz6, 0); */
/*    WRITE_LOC(x4, 0, (int)h2 + (int)1); */
/*    WRITE_LOC(x4, 1, zz3); */
/*    WRITE_LOC(x1, 0, x4); */
/*   } else { */
/*   } */
/*  } */
/* } */

void mapAdd1(loc x, loc x1) {
 if (x == 0) {
  WRITE_LOC(x1, 0, 0);
 } else {
  if (!(x == 0)) {
   loc h2 = READ_LOC(x, 0);
   loc nxt5 = READ_LOC(x, 1);
   loc x4 = (loc)malloc(2 * sizeof(loc));
   loc x6 = (loc)malloc(1 * sizeof(loc));
   mapAdd1(nxt5, x6);
   loc x3 = READ_LOC(x6, 0);
   WRITE_LOC(x4, 0, (int)h2 + (int)1);
   WRITE_LOC(x4, 1, x3);
   WRITE_LOC(x1, 0, x4);
  } else {
  }
 }
}

void mapAdd1Dll(loc x, loc z1, loc x2, loc z3) {
 if (x == 0) {
  WRITE_LOC(x2, 0, 0);
 } else {
  if (!(x == 0)) {
   loc h4 = READ_LOC(x, 0);
   loc w7 = READ_LOC(x, 1);
   loc z1 = READ_LOC(x, 2);
   loc x6 = (loc)malloc(3 * sizeof(loc));
   loc x9 = (loc)malloc(1 * sizeof(loc));
   loc z10 = (loc)malloc(1 * sizeof(loc));
   mapAdd1Dll(w7, x, x9, z10);
   loc x5 = READ_LOC(x9, 0);
   loc z8 = READ_LOC(z10, 0);
   WRITE_LOC(x6, 0, (int)h4 + (int)1);
   WRITE_LOC(x6, 1, x5);
   WRITE_LOC(x6, 2, z3);
   WRITE_LOC(x2, 0, x6);
  } else {
  }
 }
}




/* void mapAdd1(loc x, loc x1) { */
/*  if (x == 0) { */
/*   WRITE_LOC(x1, 0, 0); */
/*  } else { */
/*   if (!(x == 0)) { */
/*    loc h2 = READ_LOC(x, 0); */
/*    loc nxt5 = READ_LOC(x, 1); */
/*    loc x4 = (loc)malloc(2 * sizeof(loc)); */
/*    loc x6 = (loc)malloc(1 * sizeof(loc)); */
/*    mapAdd1(nxt5, x6); */
/*    loc x3 = READ_LOC(x6, 0); */
/*    WRITE_LOC(x4, 0, (int)h2 + (int)1); */
/*    WRITE_LOC(x4, 1, x3); */
/*    WRITE_LOC(x1, 0, x4); */
/*   } else { */
/*   } */
/*  } */
/* } */

int main() {
  loc x = malloc(sizeof(loc));
  generateRange_Sll(0, 10, x);

  loc y = malloc(sizeof(loc));

  loc list = READ_LOC(x, 0);
  mapAdd1(list, y);

  print_Sll(y);
  printf("\n");
  return 0;
}

