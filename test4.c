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
void filter(loc i1, loc x, loc x23) {
 loc x2 = (loc)malloc(1 * sizeof(loc));
 if (x == 0) {
  loc x2 = NULL;
  WRITE_LOC(x23, 0, x2);
 } else {
  if (!(x == 0)) {
   loc a = READ_LOC(x, 0);
   loc nxt17 = READ_LOC(x, 1);
   loc p9 = (loc)malloc(1 * sizeof(loc));
   loc p10 = (loc)malloc(1 * sizeof(loc));
   loc q11 = (loc)malloc(1 * sizeof(loc));
   WRITE_INT(p10, 0, a);
   WRITE_INT(q11, 0, i1);
   p10 = (long)(p10->ssl_int);
   q11 = (long)(q11->ssl_int);
   WRITE_LOC(p9, 0, p10 < q11);
   p9 = (long)(p9->ssl_int);
   if (p9) {
    loc x2 = (loc)malloc(2 * sizeof(loc));
    loc p7 = (loc)malloc(1 * sizeof(loc));
    WRITE_LOC(p7, 0, i1);
    p7 = (long)(p7->ssl_int);
    loc p8 = (loc)malloc(1 * sizeof(loc));
    WRITE_LOC(p8, 0, nxt17);
    p8 = (long)(p8->ssl_int);
    /* p8 = (loc)(p8->ssl_ptr); */
    filter(p7, p8, &x2);
    WRITE_LOC(x23, 0, x2);
   } else {
   }
  } else {
   if (!(x == 0)) {
    loc a = READ_LOC(x, 0);
    loc nxt29 = READ_LOC(x, 1);
    loc p18 = (loc)malloc(1 * sizeof(loc));
    loc p19 = (loc)malloc(1 * sizeof(loc));
    loc p20 = (loc)malloc(1 * sizeof(loc));
    loc q21 = (loc)malloc(1 * sizeof(loc));
    WRITE_LOC(p20, 0, a);
    WRITE_LOC(q21, 0, i1);
    p20 = (long)(p20->ssl_int);
    q21 = (long)(q21->ssl_int);
    WRITE_LOC(p19, 0, p20 < q21);
    p19 = (long)(p19->ssl_int);
    WRITE_LOC(p18, 0, !p19);
    p18 = (long)(p18->ssl_int);
    if (p18) {
     loc nxt1216 = (loc)malloc(2 * sizeof(loc));
     filter(i1, nxt29, &nxt1216);
     loc x2 = (loc)malloc(2 * sizeof(loc));
     WRITE_LOC(x2, 0, a);
     WRITE_LOC(x2, 1, nxt1216);
     WRITE_LOC(x23, 0, x2);
     WRITE_LOC(x23, 0, x2);
    } else {
    }
   } else {
   }
  }
 }
}
int main() {
 printf("*** Running test filter1\n");
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
 WRITE_LOC(p36, 0, 2);
 p36 = (long)(p36->ssl_int);
 loc p37 = (loc)malloc(1 * sizeof(loc));
 WRITE_LOC(p37, 0, x1934);
 p37 = (long)(p37->ssl_int);
 filter(p36, p37, &x);
 _print_Sll(x);
 printf("\n\n");
 return 0;
}
