#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>
#include <stdio.h>

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

#endif


void _print_Sll(loc x) {
 printf("(");
 if (x == 0) {
 } else {
  if (!(x == 0)) {
   loc head = READ_LOC(x, 0);
   printf("%d ", head);
   loc nxt = READ_LOC(x, 1);
   _print_Sll(nxt);
  } else {
  }
 }
 printf(")");
}
void sum(loc x, loc i12) {
 loc i1 = (loc)malloc(1 * sizeof(loc));
 if (x == 0) {
  WRITE_LOC(i1, 0, 0);
  WRITE_LOC(i12, 0, i1);
 } else {
  if (!(x == 0)) {
   loc h = READ_LOC(x, 0);
   loc nxt10 = READ_LOC(x, 1);
   loc p3 = (loc)malloc(sizeof(loc));
   loc q4 = (loc)malloc(sizeof(loc));
   WRITE_LOC(p3, 0, h);
   /* p3 = h; */
   sum(nxt10, &q4);
   p3 = (long)(p3->ssl_int);
   q4 = (long)(q4->ssl_int);
   WRITE_LOC(i1, 0, (long)p3 + (long)q4);
   WRITE_LOC(i12, 0, i1);
  } else {
  }
 }
}
int main() {
 printf("*** Running test sum\n");
 loc nxt514 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt514, 0, 2);
 WRITE_LOC(nxt514, 1, 0);
 loc x817 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x817, 0, 1);
 WRITE_LOC(x817, 1, nxt514);
 loc i = (loc)malloc(1 * sizeof(loc));
 sum(x817, &i);
 _printInt(i);
 printf("\n\n");
 return 0;
}
