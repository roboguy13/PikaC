#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>
#include <stdio.h>

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


void _print_SetLayout(loc x);
void _print_SetLayout(loc x) {
 printf("(");
 if (x == 0) {
 }
 if (!(x == 0)) {
  if (!(x == 0)) {
   loc p = READ_LOC(x, 0);
   loc v = READ_LOC(x, 1);
   printf("%d ", v);
   loc q = READ_LOC(x, 2);
   _print_SetLayout(p);
   _print_SetLayout(q);
  }
  if (!(!(x == 0))) {
  }
 }
 printf(")");
}
void member(loc i1, loc x, loc b23) {
 loc b2 = (loc)malloc(1 * sizeof(loc));
 if (x == 0) {
  WRITE_LOC(b2, 0, false);
  WRITE_LOC(b23, 0, b2);
 }
 if (!(x == 0)) {
  loc p18 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q19 = READ_LOC(x, 2);
  loc p5 = (loc)malloc(1 * sizeof(loc));
  loc p6 = (loc)malloc(1 * sizeof(loc));
  loc q7 = (loc)malloc(1 * sizeof(loc));
  p6 = &i1;
  q7 = &a;
  p6 = (long)(p6->ssl_int);
  q7 = (long)(q7->ssl_int);
  WRITE_LOC(p5, 0, p6 < q7);
  p5 = (long)(p5->ssl_int);
  if (p5) {
   loc b2 = NULL;
   member(i1, p18, &b2);
   WRITE_LOC(b23, 0, b2);
  }
 }
 if (!(x == 0)) {
  loc p35 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q36 = READ_LOC(x, 2);
  loc p8 = (loc)malloc(1 * sizeof(loc));
  loc p9 = (loc)malloc(1 * sizeof(loc));
  loc q10 = (loc)malloc(1 * sizeof(loc));
  p9 = &a;
  q10 = &i1;
  p9 = (long)(p9->ssl_int);
  q10 = (long)(q10->ssl_int);
  WRITE_LOC(p8, 0, p9 < q10);
  p8 = (long)(p8->ssl_int);
  if (p8) {
   loc b2 = NULL;
   member(i1, q36, &b2);
   WRITE_LOC(b23, 0, b2);
  }
 }
 if (!(x == 0)) {
  loc p52 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q53 = READ_LOC(x, 2);
  WRITE_LOC(b2, 0, true);
  WRITE_LOC(b23, 0, b2);
 }
}
void insert(loc i1, loc x, loc x23) {
 loc x2 = (loc)malloc(1 * sizeof(loc));
 if (x == 0) {
  loc x2 = (loc)malloc(3 * sizeof(loc));
  WRITE_LOC(x2, 0, 0);
  WRITE_LOC(x2, 1, i1);
  WRITE_LOC(x2, 2, 0);
  WRITE_LOC(x23, 0, x2);
  WRITE_LOC(x23, 0, x2);
 }
 if (!(x == 0)) {
  loc p34 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q35 = READ_LOC(x, 2);
  loc p26 = (loc)malloc(1 * sizeof(loc));
  loc p27 = (loc)malloc(1 * sizeof(loc));
  loc q28 = (loc)malloc(1 * sizeof(loc));
  p27 = &i1;
  q28 = &a;
  p27 = (long)(p27->ssl_int);
  q28 = (long)(q28->ssl_int);
  WRITE_LOC(p26, 0, p27 < q28);
  p26 = (long)(p26->ssl_int);
  if (p26) {
   loc p2024 = (loc)malloc(3 * sizeof(loc));
   insert(i1, p34, &p2024);
   loc x2 = (loc)malloc(3 * sizeof(loc));
   WRITE_LOC(x2, 0, p2024);
   WRITE_LOC(x2, 1, a);
   WRITE_LOC(x2, 2, q35);
   WRITE_LOC(x23, 0, x2);
   WRITE_LOC(x23, 0, x2);
  }
 }
 if (!(x == 0)) {
  loc p59 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q60 = READ_LOC(x, 2);
  loc p35 = (loc)malloc(1 * sizeof(loc));
  loc p36 = (loc)malloc(1 * sizeof(loc));
  loc q37 = (loc)malloc(1 * sizeof(loc));
  p36 = &a;
  q37 = &i1;
  p36 = (long)(p36->ssl_int);
  q37 = (long)(q37->ssl_int);
  WRITE_LOC(p35, 0, p36 < q37);
  p35 = (long)(p35->ssl_int);
  if (p35) {
   loc q2933 = (loc)malloc(3 * sizeof(loc));
   insert(i1, q60, &q2933);
   loc x2 = (loc)malloc(3 * sizeof(loc));
   WRITE_LOC(x2, 0, p59);
   WRITE_LOC(x2, 1, a);
   WRITE_LOC(x2, 2, q2933);
   WRITE_LOC(x23, 0, x2);
   WRITE_LOC(x23, 0, x2);
  }
 }
 if (!(x == 0)) {
  loc p84 = READ_LOC(x, 0);
  loc a = READ_LOC(x, 1);
  loc q85 = READ_LOC(x, 2);
  loc x2 = (loc)malloc(3 * sizeof(loc));
  WRITE_LOC(x2, 0, p84);
  WRITE_LOC(x2, 1, a);
  WRITE_LOC(x2, 2, q85);
  WRITE_LOC(x23, 0, x2);
  WRITE_LOC(x23, 0, x2);
 }
}
int main() {
 printf("*** Running test member1\n");
 loc ww49 = (loc)malloc(3 * sizeof(loc));
 insert(20, 0, &ww49);
 loc x = (loc)malloc(3 * sizeof(loc));
 loc p10 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p10, 0, 20);
 p10 = (long)(p10->ssl_int);
 loc p11 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p11, 0, ww49);
 p11 = (long)(p11->ssl_int);
 member(p10, p11, &x);
 _print_SetLayout(x);
 printf("\n\n");
 return 0;
}
