#ifndef GENERATORS_H
#define GENERATORS_H

#include "common.h"

void _generateInt(int* p) {
  static int i = 0;
  *p = i++;
}

void _printIntList(loc x)
{
  loc curr = READ_LOC(x, 0);

  printf("{");

  while (curr != NULL) {
    long num = (long)(READ_INT(curr, 0));

    printf("%d", num);

    loc next = (loc)READ_LOC(curr, 1);

    if (next != NULL) {
      printf(", ");
    }

    curr = next;
  }

  printf(" }");

  return;
}

void _generateIntListHelper(long start, long end, loc x) {
  loc head = malloc(sizeof(loc));

  loc n = NULL;
  for (int i = start; i < end; ++i) {
    if (i == start) {
      n = malloc(2 * sizeof(loc));

      WRITE_LOC(head, 0, n);
    }

    WRITE_LOC(n, 0, i);

    if (i < end-1) {
      loc next = malloc(2 * sizeof(loc));
      WRITE_LOC(n, 1, next);
      n = next;
    } else {
      WRITE_LOC(n, 1, NULL);
    }
  }

  loc r = READ_LOC(head, 0);
  WRITE_LOC(x, 0, r);
}

void _generateIntList(loc x) {
  generateIntListHelper(0, 9, x);
}

void _generateBinaryTree(int** tree) {
  // TODO
}

void _printIntList(loc list) {
  while (list != NULL) {
    _printInt(list);
    list = READ_LOC(list, 1);
  }
}

// Peano naturals //
void printNat(loc x) {
  loc curr = READ_LOC(x, 0);

  int i = 0;
  while (curr != NULL) {
    ++i;
    curr = (loc)READ_LOC(curr, 0);
  }

  printf("%d", i);
}

void generateNat(int num, loc x) {
  loc head = malloc(sizeof(loc));

  loc n = NULL;
  for (int i = 0; i < num; ++i) {
    if (i == 0) {
      n = malloc(sizeof(loc));
      WRITE_LOC(head, 0, n);
    }

    if (i < num-1) {
      loc next = malloc(sizeof(loc));
      WRITE_LOC(n, 0, next);
      n = next;
    } else {
      WRITE_LOC(n, 0, NULL);
    }
  }

  loc r = READ_LOC(head, 0);
  WRITE_LOC(x, 0, r);
}

#endif

