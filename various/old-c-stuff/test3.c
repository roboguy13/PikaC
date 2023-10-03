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

#endif


void _print_Sll(loc x) {
 printf("(");
 if (x == 0) {
 } else {
  if (!(x == 0)) {
   loc head = READ_LOC(x, 0);
   printf("%d ", head);
   loc nxt1 = READ_LOC(x, 1);
   _print_Sll(nxt1);
  } else {
  }
 }
 printf(")");
}
void _print_Dll(loc x, loc z) {
 printf("(");
 if ((x == 0) && (z == 0)) {
 } else {
  if ((!(x == 0)) && (z == 0)) {
   loc head = READ_LOC(x, 0);
   printf("%d ", head);
   loc w1 = READ_LOC(x, 1);
   loc z2 = READ_LOC(x, 2);
   printf("%d ", z2);
   _print_Dll(w1, x);
  } else {
  }
 }
 printf(")");
}
void mapAdd1(loc x, loc x12) {
 loc x1 = NULL;
 if (x == 0) {
  x1 = NULL;
  WRITE_LOC(x12, 0, x1);
 } else {
  if (!(x == 0)) {
   loc h = READ_LOC(x, 0);
   loc nxt161 = READ_LOC(x, 1);
   loc nxt3032 = (loc)malloc(2 * sizeof(loc));
   mapAdd1(nxt161, &nxt3032);
   loc x13 = (loc)malloc(2 * sizeof(loc));
   WRITE_LOC(x13, 0, (int)h + (int)1);
   WRITE_LOC(x13, 1, nxt3032);
   WRITE_LOC(x12, 0, x13);
  } else {
  }
 }
}
void main() {
 printf("*** Running test mapAdd1\n");
 loc x = NULL;
 loc nxt383 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt383, 0, 4);
 WRITE_LOC(nxt383, 1, 0);
 loc nxt3951 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt3951, 0, 3);
 WRITE_LOC(nxt3951, 1, nxt383);
 loc nxt4072 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt4072, 0, 2);
 WRITE_LOC(nxt4072, 1, nxt3951);
 loc x7193 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x7193, 0, 1);
 WRITE_LOC(x7193, 1, nxt4072);
 mapAdd1(x7193, &x);
 _print_Sll(x);
}
