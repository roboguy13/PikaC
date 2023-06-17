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
void revcat(loc x1, loc x, loc x23) {
 if ((x1 == 0) && (x == 0)) {
  loc x2 = NULL;
  WRITE_LOC(x23, 0, x2);
 } else {
  if ((x1 == 0) && (!(x == 0))) {
   loc h = READ_LOC(x, 0);
   loc nxt21 = READ_LOC(x, 1);
   loc x2 = (loc)malloc(2 * sizeof(loc));
   WRITE_LOC(x2, 0, h);
   WRITE_LOC(x2, 1, nxt21);
   WRITE_LOC(x23, 0, x2);
   WRITE_LOC(x23, 0, x2);
  } else {
   if ((!(x1 == 0)) && (x == 0)) {
    loc h = READ_LOC(x1, 0);
    loc nxt38 = READ_LOC(x1, 1);
    loc x1117 = (loc)malloc(2 * sizeof(loc));
    WRITE_LOC(x1117, 0, h);
    WRITE_LOC(x1117, 1, 0);
    loc x2 = (loc)malloc(2 * sizeof(loc));
    revcat(nxt38, x1117, &x2);
    WRITE_LOC(x23, 0, x2);
   } else {
    if ((!(x1 == 0)) && (!(x == 0))) {
     loc h1 = READ_LOC(x1, 0);
     loc nxt61 = READ_LOC(x1, 1);
     loc h2 = READ_LOC(x, 0);
     loc nxt62 = READ_LOC(x, 1);
     loc nxt1925 = (loc)malloc(2 * sizeof(loc));
     WRITE_LOC(nxt1925, 0, h2);
     WRITE_LOC(nxt1925, 1, nxt62);
     loc x2228 = (loc)malloc(2 * sizeof(loc));
     WRITE_LOC(x2228, 0, h1);
     WRITE_LOC(x2228, 1, nxt1925);
    loc x2 = (loc)malloc(2 * sizeof(loc));
     revcat(nxt61, x2228, &x2);
     WRITE_LOC(x23, 0, x2);
    } else {
    }
   }
  }
 }
}
int main() {
 printf("*** Running test revcat\n");
 loc x = NULL;
 loc nxt1435 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1435, 0, 2);
 WRITE_LOC(nxt1435, 1, 0);
 loc nxt1938 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1938, 0, 20);
 WRITE_LOC(nxt1938, 1, 0);
 loc x2341 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x2341, 0, 1);
 WRITE_LOC(x2341, 1, nxt1435);
 loc x2644 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x2644, 0, 10);
 WRITE_LOC(x2644, 1, nxt1938);
 revcat(x2341, x2644, &x);
 _print_Sll(x);
 printf("\n");
 return 0;
}
