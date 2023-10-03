#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define true 1
#define false 0

typedef union sslval
{
    long ssl_int;
    void *ssl_ptr;
} *loc;

#define READ_LOC(x, y) (*(x + y)).ssl_ptr
#define READ_INT(x, y) (*(x + y)).ssl_int
#define WRITE_LOC(x, y, z) (*(x + y)).ssl_ptr = z
#define WRITE_INT(x, y, z) (*(x + y)).ssl_int = z

void _printInt(loc x) {
  printf("%d", x->ssl_int);
}

void _printBool(loc x) {
  printf("%d", x->ssl_int);
}

#endif


void _print_Sll(loc x);
void _print_Sll(loc x) {
 printf("(");
 if (x == 0) {
 }
 if (!(x == 0)) {
  if (!(x == 0)) {
   loc head = READ_LOC(x, 0);
   printf("%d ", head);
   loc nxt = READ_LOC(x, 1);
   _print_Sll(nxt);
  }
  if (!(!(x == 0))) {
  }
 }
 printf(")");
}
void filter(loc i1, loc x, loc x2);
void filter(loc i1, loc x, loc x2) {
 if (0 < i1) {
  if (x == 0) {
   WRITE_LOC(x2, 0, 0);
  } else {
   loc n = READ_LOC(x, 1);
   filter(i1, n, x2);
   WRITE_LOC(x, 0, 0);
  }
 } else {
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
}
/* void filter(loc i1, loc x, loc x2) { */
/*  loc _x2 = (loc)malloc(1 * sizeof(loc)); */
/*  loc _i1 = NULL; */
/*  loc _x = NULL; */
/*  WRITE_LOC(_i1, 0, i1); */
/*  WRITE_LOC(_x, 0, x); */
/*  _filter(_i1, _x, _x2); */
/*  WRITE_LOC(x2, 0, _x2); */
/* } */

/* void filter(loc i1, loc x, loc x2) { */
/*  loc _x2 = (loc)malloc(1 * sizeof(loc)); */
/*  loc _i1 = NULL; */
/*  loc _x = NULL; */
/*  WRITE_LOC(_i1, 0, i1); */
/*  WRITE_LOC(_x, 0, x); */
/*  _filter(_i1, _x, _x2); */
/*  WRITE_LOC(x2, 0, _x2); */
/* } */
/* void filter(loc i1, loc x, loc x23) { */
/*  loc x2 = (loc)malloc(1 * sizeof(loc)); */
/*  if (x == 0) { */
/*   loc x2 = NULL; */
/*   WRITE_LOC(x23, 0, x2); */
/*  } */
/*  if (!(x == 0)) { */
/*   loc a = READ_LOC(x, 0); */
/*   loc nxt17 = READ_LOC(x, 1); */
/*   loc p7 = (loc)malloc(1 * sizeof(loc)); */
/*   loc p8 = (loc)malloc(1 * sizeof(loc)); */
/*   loc q9 = (loc)malloc(1 * sizeof(loc)); */
/*   p8 = &a; */
/*   q9 = &i1; */
/*   p8 = (long)(p8->ssl_int); */
/*   q9 = (long)(q9->ssl_int); */
/*   WRITE_LOC(p7, 0, p8 < q9); */
/*   p7 = (long)(p7->ssl_int); */
/*   if (p7) { */
/*    loc x2 = (loc)malloc(2 * sizeof(loc)); */
/*    filter(i1, nxt17, &x2); */
/*    WRITE_LOC(x23, 0, x2); */
/*   } */
/*  } */
/*  if (!(x == 0)) { */
/*   loc a = READ_LOC(x, 0); */
/*   loc nxt29 = READ_LOC(x, 1); */
/*   loc p16 = (loc)malloc(1 * sizeof(loc)); */
/*   loc p17 = (loc)malloc(1 * sizeof(loc)); */
/*   loc p18 = (loc)malloc(1 * sizeof(loc)); */
/*   loc q19 = (loc)malloc(1 * sizeof(loc)); */
/*   p18 = &a; */
/*   q19 = &i1; */
/*   p18 = (long)(p18->ssl_int); */
/*   q19 = (long)(q19->ssl_int); */
/*   WRITE_LOC(p17, 0, p18 < q19); */
/*   p17 = (long)(p17->ssl_int); */
/*   WRITE_LOC(p16, 0, !p17); */
/*   p16 = (long)(p16->ssl_int); */
/*   if (p16) { */
/*    loc nxt1014 = (loc)malloc(2 * sizeof(loc)); */
/*    filter(i1, nxt29, &nxt1014); */
/*    loc x2 = (loc)malloc(2 * sizeof(loc)); */
/*    WRITE_LOC(x2, 0, a); */
/*    WRITE_LOC(x2, 1, nxt1014); */
/*    WRITE_LOC(x23, 0, x2); */
/*    WRITE_LOC(x23, 0, x2); */
/*   } */
/*  } */
/* } */
void length2(loc x1, loc x, loc i23) {
 loc i2 = (loc)malloc(1 * sizeof(loc));
 if ((x1 == 0) && (x == 0)) {
  WRITE_LOC(i2, 0, 0);
  WRITE_LOC(i23, 0, i2);
 }
 if ((!(x1 == 0)) && (x == 0)) {
  loc b = READ_LOC(x1, 0);
  loc nxt17 = READ_LOC(x1, 1);
  WRITE_LOC(i2, 0, 0);
  WRITE_LOC(i23, 0, i2);
 }
 if ((x1 == 0) && (!(x == 0))) {
  loc a = READ_LOC(x, 0);
  loc nxt28 = READ_LOC(x, 1);
  loc p8 = (loc)malloc(1 * sizeof(loc));
  loc q9 = (loc)malloc(1 * sizeof(loc));
  WRITE_LOC(p8, 0, 1);
  loc x610 = NULL;
  length2(x610, nxt28, &q9);
  p8 = (long)(p8->ssl_int);
  q9 = (long)(q9->ssl_int);
  WRITE_LOC(i2, 0, (long)p8 + (long)q9);
  WRITE_LOC(i23, 0, i2);
 }
 if ((!(x1 == 0)) && (!(x == 0))) {
  loc b = READ_LOC(x1, 0);
  loc nxt45 = READ_LOC(x1, 1);
  loc a = READ_LOC(x, 0);
  loc nxt46 = READ_LOC(x, 1);
  loc p14 = (loc)malloc(1 * sizeof(loc));
  loc q15 = (loc)malloc(1 * sizeof(loc));
  WRITE_LOC(p14, 0, 1);
  loc x1216 = NULL;
  length2(x1216, nxt46, &q15);
  p14 = (long)(p14->ssl_int);
  q15 = (long)(q15->ssl_int);
  WRITE_LOC(i2, 0, (long)p14 + (long)q15);
  WRITE_LOC(i23, 0, i2);
 }
}
void length2Filter(loc x, loc i12) {
 loc i1 = (loc)malloc(1 * sizeof(loc));
 if (x == 0) {
  WRITE_LOC(i1, 0, 0);
  WRITE_LOC(i12, 0, i1);
 }
 if (!(x == 0)) {
  loc a = READ_LOC(x, 0);
  loc nxt10 = READ_LOC(x, 1);
  loc x413 = (loc)malloc(2 * sizeof(loc));
  WRITE_LOC(x413, 0, a);
  WRITE_LOC(x413, 1, nxt10);
  loc ww816 = (loc)malloc(2 * sizeof(loc));
  filter(5, x413, &ww816);
  loc x1018 = (loc)malloc(2 * sizeof(loc));
  WRITE_LOC(x1018, 0, a);
  WRITE_LOC(x1018, 1, nxt10);
  loc i1 = NULL;
  loc p20 = (loc)malloc(1 * sizeof(loc));
  WRITE_LOC(p20, 0, ww816);
  p20 = (long)(p20->ssl_int);
  length2(p20, x1018, &i1);
  WRITE_LOC(i12, 0, i1);
 }
}
int main() {
 printf("*** Running test filter_1\n");
 loc nxt725 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt725, 0, 20);
 WRITE_LOC(nxt725, 1, 0);
 loc nxt1228 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1228, 0, 10);
 WRITE_LOC(nxt1228, 1, nxt725);
 loc nxt1631 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1631, 0, 2);
 WRITE_LOC(nxt1631, 1, nxt1228);
 loc x1934 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x1934, 0, 1);
 WRITE_LOC(x1934, 1, nxt1631);
 loc x = (loc)malloc(2 * sizeof(loc));
 loc p36 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p36, 0, 5);
 p36 = (long)(p36->ssl_int);
 filter(p36, x1934, &x);
 _print_Sll(x);
 printf("\n\n");
 printf("*** Running test length2_1\n");
 loc nxt5172 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt5172, 0, 20);
 WRITE_LOC(nxt5172, 1, 0);
 loc nxt5675 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt5675, 0, 10);
 WRITE_LOC(nxt5675, 1, nxt5172);
 loc nxt6078 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt6078, 0, 2);
 WRITE_LOC(nxt6078, 1, nxt5675);
 loc x6381 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x6381, 0, 1);
 WRITE_LOC(x6381, 1, nxt6078);
 loc i37 = NULL;
 loc p83 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p83, 0, 0);
 p83 = (long)(p83->ssl_int);
 length2(p83, x6381, &i37);
 _printInt(i37);
 printf("\n\n");
 printf("*** Running test length2Filter\n");
 loc nxt91109 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt91109, 0, 20);
 WRITE_LOC(nxt91109, 1, 0);
 loc nxt96112 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt96112, 0, 10);
 WRITE_LOC(nxt96112, 1, nxt91109);
 loc nxt100115 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt100115, 0, 2);
 WRITE_LOC(nxt100115, 1, nxt96112);
 loc x103118 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x103118, 0, 1);
 WRITE_LOC(x103118, 1, nxt100115);
 loc i84 = NULL;
 length2Filter(x103118, &i84);
 _printInt(i84);
 printf("\n\n");
 return 0;
}
