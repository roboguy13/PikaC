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
void _print_NatLayout(loc x) {
 printf("(");
 if (x == 0) {
 } else {
  if (!(x == 0)) {
   loc nxt = READ_LOC(x, 0);
   _print_NatLayout(nxt);
  } else {
  }
 }
 printf(")");
}
void take(loc x1, loc x, loc x23) {
 if ((x1 == 0) && (x == 0)) {
  loc x2 = NULL;
  WRITE_LOC(x23, 0, x2);
 } else {
  if ((!(x1 == 0)) && (x == 0)) {
   loc nxt20 = READ_LOC(x1, 0);
   loc x2 = NULL;
   WRITE_LOC(x23, 0, x2);
  } else {
   if ((x1 == 0) && (!(x == 0))) {
    loc h = READ_LOC(x, 0);
    loc nxt32 = READ_LOC(x, 1);
    loc x2 = NULL;
    WRITE_LOC(x23, 0, x2);
   } else {
    if ((!(x1 == 0)) && (!(x == 0))) {
     loc nxt46 = READ_LOC(x1, 0);
     loc h = READ_LOC(x, 0);
     loc nxt47 = READ_LOC(x, 1);
     loc nxt48 = (loc)malloc(2 * sizeof(loc));
     take(nxt46, nxt47, &nxt48);
     loc x2 = (loc)malloc(2 * sizeof(loc));
     WRITE_LOC(x2, 0, h);
     WRITE_LOC(x2, 1, nxt47);
     WRITE_LOC(x23, 0, x2);
     WRITE_LOC(x23, 0, x2);
    } else {
    }
   }
  }
 }
}
int main() {
 printf("*** Running test take\n");
 loc x = NULL;
 loc nxt1849 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(nxt1849, 0, 0);
 loc nxt2551 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt2551, 0, 40);
 WRITE_LOC(nxt2551, 1, 0);
 loc nxt3153 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt3153, 0, 30);
 WRITE_LOC(nxt3153, 1, nxt2551);
 loc nxt3655 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt3655, 0, 20);
 WRITE_LOC(nxt3655, 1, nxt3153);
 loc x4057 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(x4057, 0, nxt1849);
 loc x4359 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x4359, 0, 10);
 WRITE_LOC(x4359, 1, nxt3655);
 take(x4057, x4359, &x);
 _print_Sll(x);
 printf("\n");
 return 0;
}
