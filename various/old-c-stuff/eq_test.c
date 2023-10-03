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
void equal(loc x1, loc x, loc b2) {
 if (x1 == 0) {
  if (x == 0) {
   WRITE_LOC(b2, 0, 1);
  } else {
   WRITE_LOC(b2, 0, 0);
  }
 } else {
  loc h = READ_LOC(x1, 0);
  loc nx = READ_LOC(x1, 1);
  if (x == 0) {
   WRITE_LOC(b2, 0, 0);
  } else {
   loc n = READ_LOC(x, 1);
   equal(n, nx, b2);
   loc b = READ_LOC(b2, 0);
   if (b == 0) {
    WRITE_LOC(x, 1, nx);
    WRITE_LOC(x1, 1, n);
   } else {
    WRITE_LOC(x, 1, nx); WRITE_LOC(x1, 1, n); WRITE_LOC(x, 0, h);
   }
  }
 }
}
int main() {
 printf("*** Running test equal1\n");
 loc nxt1435 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1435, 0, 2);
 WRITE_LOC(nxt1435, 1, 0);
 loc nxt1938 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt1938, 0, 3);
 WRITE_LOC(nxt1938, 1, 0);
 loc x2341 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x2341, 0, 1);
 WRITE_LOC(x2341, 1, nxt1435);
 loc x2644 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x2644, 0, 2);
 WRITE_LOC(x2644, 1, nxt1938);
 loc b = NULL;
 equal(x2341, x2644, &b);
 _printBool(b);
 printf("\n\n");
 printf("*** Running test equal2\n");
 loc nxt5668 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt5668, 0, 2);
 WRITE_LOC(nxt5668, 1, 0);
 loc x5971 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x5971, 0, 1);
 WRITE_LOC(x5971, 1, nxt5668);
 loc b46 = NULL;
 loc p73 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p73, 0, 0);
 p73 = (long)(p73->ssl_int);
 equal(x5971, p73, &b46);
 _printBool(b46);
 printf("\n\n");
 printf("*** Running test equal3\n");
 loc nxt88109 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt88109, 0, 3);
 WRITE_LOC(nxt88109, 1, 0);
 loc nxt93112 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(nxt93112, 0, 3);
 WRITE_LOC(nxt93112, 1, 0);
 loc x97115 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x97115, 0, 2);
 WRITE_LOC(x97115, 1, nxt88109);
 loc x100118 = (loc)malloc(2 * sizeof(loc));
 WRITE_LOC(x100118, 0, 2);
 WRITE_LOC(x100118, 1, nxt93112);
 loc b74 = NULL;
 equal(x97115, x100118, &b74);
 _printBool(b74);
 printf("\n\n");
 return 0;
}
