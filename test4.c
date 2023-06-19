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
void max(loc i1, loc i, loc i23) {
 loc i2 = (loc)malloc(1 * sizeof(loc));
 if (true) {
  loc p4 = (loc)malloc(1 * sizeof(loc));
  loc p5 = (loc)malloc(1 * sizeof(loc));
  loc q6 = (loc)malloc(1 * sizeof(loc));
  p5 = i1;
  q6 = i;
  p5 = (long)(p5->ssl_int);
  q6 = (long)(q6->ssl_int);
  WRITE_LOC(p4, 0, p5 < q6);
  p4 = (long)(p4->ssl_int);
  if (p4) {
   WRITE_LOC(i2, 0, i->ssl_int);
   WRITE_LOC(i23, 0, i2);
  }
 }
 if (true) {
  loc p7 = (loc)malloc(1 * sizeof(loc));
  loc p8 = (loc)malloc(1 * sizeof(loc));
  loc p9 = (loc)malloc(1 * sizeof(loc));
  loc q10 = (loc)malloc(1 * sizeof(loc));
  p9 = i1;
  q10 = i;
  p9 = (long)(p9->ssl_int);
  q10 = (long)(q10->ssl_int);
  WRITE_LOC(p8, 0, p9 < q10);
  p8 = (long)(p8->ssl_int);
  WRITE_LOC(p7, 0, !p8);
  p7 = (long)(p7->ssl_int);
  if (p7) {
   WRITE_LOC(i2, 0, i1->ssl_int);
   WRITE_LOC(i23, 0, i2);
  }
 }
}
int main() {
 printf("*** Running test max1\n");
 loc i = (loc)malloc(1 * sizeof(loc));
 loc p1 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p1, 0, 1);
 p1 = (long)(p1->ssl_int);
 loc p2 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p2, 0, 3);
 p2 = (long)(p2->ssl_int);
 max(p1, p2, &i);
 _printInt(i);
 printf("\n\n");
 printf("*** Running test max2\n");
 loc i3 = (loc)malloc(1 * sizeof(loc));
 loc p4 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p4, 0, 10);
 p4 = (long)(p4->ssl_int);
 loc p5 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p5, 0, 7);
 p5 = (long)(p5->ssl_int);
 max(p4, p5, &i3);
 _printInt(i3);
 printf("\n\n");
 return 0;
}
