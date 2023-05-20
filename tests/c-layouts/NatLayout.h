#include "common/common.h"

void print_NatLayout(loc x) {
  loc curr = READ_LOC(x, 0);

  int i = 0;
  while (curr != NULL) {
    ++i;
    curr = (loc)READ_LOC(curr, 0);
  }

  printf("%d", i);
}

/*
take {}  {} ==> { x7 } := layout { x7 } {};
take {x4 :-> n10}  {} ==> { x7 } := layout { x7 } {};
take {}  {x :-> h13 ** (x + 1) :-> t14} ==> { x7 } :=
      layout { x7 } {x7 :-> h13 ** (x7 + 1) :-> t14};
take {x4 :-> n17}  {x :-> h18 ** (x + 1) :-> t19} ==> { x7 } :=
      with { <zz25> } := take n17 t19
      in
      layout { x7 } {x7 :-> h18 ** (x7 + 1) :-> zz25};
*/

void generate_NatLayout(int num, loc x) {
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

