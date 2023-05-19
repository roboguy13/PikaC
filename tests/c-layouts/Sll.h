#include "common/common.h"

void print_Sll(loc x)
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

void generateRange_Sll(long start, long end, loc x) {
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

