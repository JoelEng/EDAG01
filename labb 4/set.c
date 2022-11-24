#include "set.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

set_t *new_set(node_t *node) {
  set_t *set;

  set = malloc(sizeof(set_t));

  set->succ = NULL;
  set->node = node;

  return set;
}

bool add(set_t *set, node_t *node) {
  set_t *current = set;
  if (current->node == node) {
    return false;
  }
  while (current->succ != NULL) {
    if (current->succ->node == node) {
      return false;
    }
    current = current->succ;
  }
  current->succ = new_set(node);
  return true;
}

bool remove_node(set_t *set, node_t *node) {
  set_t *current = set;
  set_t *temp;

  if (set->node == node) {
    *set = *set->succ;
    return true;
  }

  while (current->succ != NULL) {
    if (current->succ->node == node) {
      temp = current->succ;
      current->succ = current->succ->succ;
      return true;
    }
    current = current->succ;
  }
  return false;
}

// int main() {
//   node_t *node = malloc(sizeof(node_t));
//   node->m = 1;
//   set_t *s = new_set(node);

//   node_t *node2 = malloc(sizeof(node_t));
//   node2->m = 10;
//   add(s, node2);

//   node_t *node3 = malloc(sizeof(node_t));
//   node3->m = 20;
//   add(s, node3);

//   if (remove_node(s, node)) {
//     printf("wow %d\n", s->node->m);
//     printf("wow succ %d\n", s->succ->node->m);
//   }
// }
