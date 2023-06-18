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

