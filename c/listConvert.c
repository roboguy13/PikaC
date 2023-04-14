#include <stdlib.h>
#include <stdio.h>

typedef struct Sll {
  int x_0;
  struct Sll* x_1;
} Sll;

typedef struct Dll {
  int x_0;
  struct Dll* x_1;
  struct Dll* x_2;
} Dll;

void convertList1(Dll* arg, Sll** r) {
  if (arg == 0) {
      // pattern match {}

      // ssl {}
    *r = 0;
  } else {
      // pattern match {x :-> h, (x+1) :-> w, (x+2) :-> z}
    int h = arg->x_0;
    Dll* w = arg->x_1;
    Dll* z = arg->x_2;

      // allocations for result
    *r = malloc(sizeof(Sll));

      // with {nxt} := convertList1 {w x} in ...
    Sll* nxt = 0;
    convertList1(arg, &nxt);

      // ssl {y :-> h, (y+1) :-> nxt}
    (*r)->x_0 = h;
    (*r)->x_1 = nxt;
  }
}

void convertList2(Sll* arg, Dll** r, Dll** z) {
  if (arg == 0) {
      // pattern match {}

      // ssl {}
    *r = 0;
  } else {
      // pattern match {y :-> h, (y+1) :-> nxt}
    int h = arg->x_0;
    Sll* nxt = arg->x_1;

      // allocations for result
    *r = malloc(sizeof(Dll));

      // with {w x} := convertList2 {nxt} in ...
    Dll* w = 0;
    convertList2(nxt, &w, r);

      // ssl {x :-> h, (x+1) :-> w, (x+2) :-> z}
    (*r)->x_0 = h;
    (*r)->x_1 = w;
    (*r)->x_2 = *z;
  }
}

Sll* generateSll(int start, int end) {
  Sll* n = malloc(sizeof(Sll));
  Sll* original = n;

  for (int i = start; i < end; ++i) {
    n->x_0 = i;

    if (i != end-1) {
      n->x_1 = malloc(sizeof(Sll));
      n = n->x_1;
    }
  }

  n->x_1 = 0;
  return original;
}

void printSll(Sll* n) {
  printf("[");

  Sll* curr = n;

  while (curr != 0) {
    printf("%d", curr->x_0);

    if (curr->x_1 != 0) {
      printf(", ");
    }

    curr = curr->x_1;
  }

  printf("]");
}

void printDll(Dll* n) {
  printf("[\n");

  Dll* curr = n;

  while (curr != 0) {
    printf("%p: %d (back-ptr: %p)", curr, curr->x_0, curr->x_2);

    curr = curr->x_1;
    printf("\n");
  }

  printf("]");
}

int main() {
  Sll* n = generateSll(0, 10);
  printSll(n);
  printf("\n");

  Dll* d = 0;
  Dll* z = 0;
  convertList2(n, &d, &z);

  printDll(d);

  printf("\n");

  return 0;
}

