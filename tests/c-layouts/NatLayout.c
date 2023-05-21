#include "NatLayout.h"
#include "Sll.h"
void take(loc x, loc x1, loc x2) {
 if ((x == 0) && (x1 == 0)) {
  WRITE_LOC(x2, 0, 0);
 } else {
  if ((!(x == 0)) && (x1 == 0)) {
   WRITE_LOC(x2, 0, 0);
   loc nxt6 = READ_LOC(x, 0);
  } else {
   if ((x == 0) && (!(x1 == 0))) {
    WRITE_LOC(x2, 0, 0);
    loc h9 = READ_LOC(x1, 0);
    loc nxt7 = READ_LOC(x1, 1);
   } else {
    if ((!(x == 0)) && (!(x1 == 0))) {
     loc nxt6 = READ_LOC(x, 0);
     loc h3 = READ_LOC(x1, 0);
     loc nxt7 = READ_LOC(x1, 1);
     loc x5 = (loc)malloc(2 * sizeof(loc));
     loc x8 = (loc)malloc(1 * sizeof(loc));
     take(nxt6, nxt7, x8);
     loc x4 = READ_LOC(x8, 0);
     WRITE_LOC(x5, 0, h3);
     WRITE_LOC(x5, 1, x4);
     WRITE_LOC(x2, 0, x5);
    } else {
    }
   }
  }
 }
}

/* void take(loc x, loc x1, loc x2) { */
/*  if ((x == 0) && (x1 == 0)) { */
/*   WRITE_LOC(x2, 0, 0); */
/*  } else { */
/*   if ((!(x == 0)) && (x1 == 0)) { */
/*    WRITE_LOC(x2, 0, 0); */
/*    loc nxt6 = READ_LOC(x, 0); */
/*   } else { */
/*    if ((x == 0) && (!(x1 == 0))) { */
/*     WRITE_LOC(x2, 0, 0); */
/*     loc h9 = READ_LOC(x1, 0); */
/*     loc nxt7 = READ_LOC(x1, 1); */
/*    } else { */
/*     if ((!(x == 0)) && (!(x1 == 0))) { */
/*      loc nxt6 = READ_LOC(x, 0); */
/*      loc h3 = READ_LOC(x1, 0); */
/*      loc nxt7 = READ_LOC(x1, 1); */
/*      loc x5 = (loc)malloc(2 * sizeof(loc)); */
/*      loc x8 = (loc)malloc(1 * sizeof(loc)); */
/*      take(nxt6, nxt7, x8); */
/*      loc x4 = READ_LOC(x8, 0); */
/*      WRITE_LOC(x5, 0, h3); */
/*      WRITE_LOC(x5, 1, x4); */
/*      WRITE_LOC(x2, 0, x5); */
/*     } else { */
/*     } */
/*    } */
/*   } */
/*  } */
/* } */


/* void take(loc x, loc x1) { */
/*  if (x == 0) { */
/*   WRITE_LOC(x1, 0, 0); */
/*  } else { */
/*   if (!(x == 0)) { */
/*    WRITE_LOC(x1, 0, 0); */
/*    loc nxt5 = READ_LOC(x, 0); */
/*   } else { */
/*    if (!(x == 0)) { */
/*     WRITE_LOC(x1, 0, 0); */
/*     loc h7 = READ_LOC(x, 0); */
/*     loc nxt5 = READ_LOC(x, 1); */
/*    } else { */
/*     if (!(x == 0)) { */
/*      loc nxt5 = READ_LOC(x, 0); */
/*      loc h2 = READ_LOC(x, 0); */
/*      loc nxt5 = READ_LOC(x, 1); */
/*      loc x4 = (loc)malloc(2 * sizeof(loc)); */
/*      loc x6 = (loc)malloc(1 * sizeof(loc)); */
/*      take(nxt5, nxt5, x6); */
/*      loc x3 = READ_LOC(x6, 0); */
/*      WRITE_LOC(x4, 0, h2); */
/*      WRITE_LOC(x4, 1, x3); */
/*      WRITE_LOC(x1, 0, x4); */
/*     } else { */
/*     } */
/*    } */
/*   } */
/*  } */
/* } */

/* void take(loc x, loc x1, loc x2) { */
/*  if ((x == 0) && (x1 == 0)) { */
/*   WRITE_LOC(x2, 0, 0); */
/*  } else { */
/*   if ((!(x == 0)) && (x1 == 0)) { */
/*    WRITE_LOC(x2, 0, 0); */
/*    loc n6 = READ_LOC(x, 0); */
/*   } else { */
/*    if ((x == 0) && (!(x1 == 0))) { */
/*     WRITE_LOC(x2, 0, 0); */
/*     loc h3 = READ_LOC(x1, 0); */
/*     loc t7 = READ_LOC(x1, 1); */
/*    } else { */
/*     if ((!(x == 0)) && (!(x1 == 0))) { */
/*      loc n6 = READ_LOC(x, 0); */
/*      loc h3 = READ_LOC(x1, 0); */
/*      loc t7 = READ_LOC(x1, 1); */
/*      loc x5 = (loc)malloc(2 * sizeof(loc)); */
/*      loc zz8 = (loc)malloc(1 * sizeof(loc)); */
/*      take(n6, t7, zz8); */
/*      loc zz4 = READ_LOC(zz8, 0); */
/*      WRITE_LOC(x5, 0, h3); */
/*      WRITE_LOC(x5, 1, zz4); */
/*      WRITE_LOC(x2, 0, x5); */
/*     } else { */
/*     } */
/*    } */
/*   } */
/*  } */
/* } */

/* void take(loc x, loc x1, loc x2) { */
/*  if ((x == 0) && (x1 == 0)) { // Z Nil */
/*   WRITE_LOC(x2, 0, 0); */
/*  } else { */
/*   if ((x == 0) && (!(x1 == 0))) { // S Nil */
/*    WRITE_LOC(x2, 0, 0); */
/*    loc n12 = READ_LOC(x1, 0); */
/*   } else { */
/*    if ((!(x == 0)) && (x1 == 0)) { // Z Cons */
/*      #<{(| WRITE_LOC(x2, 0, 0); |)}># */
/*     loc h9 = READ_LOC(x, 0); */
/*     loc t10 = READ_LOC(x, 1); */
/*     loc x11 = (loc)malloc(2 * sizeof(loc)); */
/*     WRITE_LOC(x11, 0, h9); */
/*     WRITE_LOC(x11, 1, t10); */
/*     WRITE_LOC(x2, 0, x11); */
/*    } else { // S Cons */
/*     if ((!(x == 0)) && (!(x1 == 0))) { */
/*      loc n6 = READ_LOC(x, 0); */
/*      loc h3 = READ_LOC(x1, 0); */
/*      loc t7 = READ_LOC(x1, 1); */
/*      loc x5 = (loc)malloc(2 * sizeof(loc)); */
/*      loc zz8 = (loc)malloc(1 * sizeof(loc)); */
/*      take(n6, t7, zz8); */
/*      loc zz4 = READ_LOC(zz8, 0); */
/*      WRITE_LOC(x5, 0, h3); */
/*      WRITE_LOC(x5, 1, zz4); */
/*      WRITE_LOC(x2, 0, x5); */
/*     } else { */
/*     } */
/*    } */
/*   } */
/*  } */
/* } */

/* void take(loc x, loc x1, loc x2) { */
/*  if ((x1 == 0) && ((x == 0) && ((x1 == 0) && (x == 0)))) { */
/*   WRITE_LOC(x2, 0, 0); */
/*  } else { */
/*   if ((!(x1 == 0)) && ((x == 0) && ((!(x1 == 0)) && (x == 0)))) { */
/*    WRITE_LOC(x2, 0, 0); */
/*    loc n12 = READ_LOC(x1, 0); */
/*   } else { */
/*    if ((x1 == 0) && ((!(x == 0)) && ((x1 == 0) && (!(x == 0))))) { */
/*     loc h9 = READ_LOC(x, 0); */
/*     loc t10 = READ_LOC(x, 1); */
/*     loc x11 = (loc)malloc(2 * sizeof(loc)); */
/*     WRITE_LOC(x11, 0, h9); */
/*     WRITE_LOC(x11, 1, t10); */
/*     WRITE_LOC(x2, 0, x11); */
/*    } else { */
/*     if ((!(x1 == 0)) */
/*         && */
/*         ((!(x == 0)) && ((!(x1 == 0)) && (!(x == 0))))) { */
/*      loc n6 = READ_LOC(x1, 0); */
/*      loc h3 = READ_LOC(x, 0); */
/*      loc t7 = READ_LOC(x, 1); */
/*      loc x5 = (loc)malloc(2 * sizeof(loc)); */
/*      loc zz8 = (loc)malloc(1 * sizeof(loc)); */
/*      take(n6, t7, zz8); */
/*      loc zz4 = READ_LOC(zz8, 0); */
/*      WRITE_LOC(x5, 0, h3); */
/*      WRITE_LOC(x5, 1, zz4); */
/*      WRITE_LOC(x2, 0, x5); */
/*     } else { */
/*     } */
/*    } */
/*   } */
/*  } */
/* } */

int main() {
  loc x = malloc(sizeof(loc));
  generate_NatLayout(3, x);
  print_NatLayout(x);
  printf("\n");

  loc y = malloc(sizeof(loc));
  generateRange_Sll(0, 10, y);
  print_Sll(y);
  printf("\n");

  loc nat = READ_LOC(x, 0);
  loc list = READ_LOC(y, 0);

  loc z = malloc(sizeof(loc));
  take(nat, list, z);

  print_Sll(z);
  printf("\n");
}
