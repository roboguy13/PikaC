#ifndef GENERATORS_H
#define GENERATORS_H

#include "common.h"
#include "../../haskell/Defaults.hs"

loc _generateInt() {
  static int i = 0;
  loc p = (loc)malloc(sizeof(loc));
  WRITE_INT(p, 0, i);
  return p;
}

void _printIntList(loc x)
{
    if (x == 0) return;

    printf("[");

    loc curr = x;  // Start from the head of the list

    while (curr != NULL) {
        long num = READ_INT(curr, 0);  // Read the integer value

        printf("%ld", num);  // Print the integer value

        curr = (loc)READ_LOC(curr, 1);  // Move to the next node

        if (curr != NULL) {
            printf(",");
        }
    }

    printf("]");

    return;
}

// void _printIntList(loc x)
// {
//   if (x == 0) return;
//
//   loc curr = READ_LOC(x, 0);
//
//   printf("{");
//
//   while (curr != NULL) {
//     long num = (long)(READ_INT(curr, 0));
//
//     printf("%d", num);
//
//     loc next = (loc)READ_LOC(curr, 1);
//
//     if (next != NULL) {
//       printf(", ");
//     }
//
//     curr = next;
//   }
//
//   printf(" }");
//
//   return;
// }

loc _generateIntListHelper(int i, int len) {
  if (len == 0) return NULL;

  loc head = (loc)malloc(2 * sizeof(loc));
  WRITE_INT(head, 0, i);
  WRITE_LOC(head, 1, _generateIntListHelper(i + 1, len - 1));
    
  return head;
}

loc _generateIntList() {
  return _generateIntListHelper(0, LIST_MAX);
}

// void _generateIntListHelper(long start, long end, loc x) {
//   loc head = malloc(sizeof(loc));
//
//   loc n = NULL;
//   for (int i = start; i < end; ++i) {
//     if (i == start) {
//       n = malloc(2 * sizeof(loc));
//
//       WRITE_LOC(head, 0, n);
//     }
//
//     WRITE_LOC(n, 0, i);
//
//     if (i < end-1) {
//       loc next = malloc(2 * sizeof(loc));
//       WRITE_LOC(n, 1, next);
//       n = next;
//     } else {
//       WRITE_LOC(n, 1, NULL);
//     }
//   }
//
//   loc r = READ_LOC(head, 0);
//   WRITE_LOC(x, 0, r);
// }
//
// void _generateIntList(loc x) {
//   _generateIntListHelper(0, LIST_MAX, x);
// }
//

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

loc _generateNatN(int n) {
  if (n == 0) return NULL;

  loc node = (loc)malloc(2 * sizeof(loc));
  WRITE_LOC(node, 0, _generateNatN(n - 1));
    
  return node;
}

loc _generateNat() {
  return _generateNatN(DEFAULT_NAT);
}

loc createBinaryTree(int depth) {
    if (depth == 0) {
      return NULL;
    } else {
      loc node = malloc(3 * sizeof(loc));
      WRITE_INT(node, 0, depth);
      WRITE_LOC(node, 1, createBinaryTree(depth - 1));
      WRITE_LOC(node, 2, createBinaryTree(depth - 1));
      return node;
    }
}

loc _generateBinaryTree() {
  return createBinaryTree(BINARY_TREE_SIZE);
}

#endif

