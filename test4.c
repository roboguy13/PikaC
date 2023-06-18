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
   loc nxt = READ_LOC(x, 1);
   _print_Sll(nxt);
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
   loc w = READ_LOC(x, 1);
   loc z = READ_LOC(x, 2);
   printf("%d ", z);
   _print_Dll(w, x);
  } else {
  }
 }
 printf(")");
}
void mapAdd1(loc x, loc x12) {
 if (x == 0) {
  loc x1 = NULL;
  WRITE_LOC(x12, 0, x1);
 } else {
  if (!(x == 0)) {
   loc h = READ_LOC(x, 0);
   loc nxt16 = READ_LOC(x, 1);
   loc nxt59 = (loc)malloc(2 * sizeof(loc));
   mapAdd1(nxt16, &nxt59);
   loc x1 = (loc)malloc(2 * sizeof(loc));
   WRITE_LOC(x1, 0, (int)h + (int)1);
   WRITE_LOC(x1, 1, nxt59);
   WRITE_LOC(x12, 0, x1);
   WRITE_LOC(x12, 0, x1);
  } else {
  }
 }
}
int main() {
 printf("*** Running test mapAdd1\n");
 loc nxt725 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt725, 0, 4);
 WRITE_LOC(nxt725, 1, 0);
 loc nxt1228 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1228, 0, 3);
 WRITE_LOC(nxt1228, 1, nxt725);
 loc nxt1631 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1631, 0, 2);
 WRITE_LOC(nxt1631, 1, nxt1228);
 loc x1934 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x1934, 0, 1);
 WRITE_LOC(x1934, 1, nxt1631);
 loc x = (loc)malloc(2 * sizeof(loc));
 mapAdd1(x1934, &x);
 _print_Sll(x);
 printf("\n\n");
 return 0;
}
