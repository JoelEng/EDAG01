#include "node.h"
#include <stdbool.h>

typedef struct set_t set_t;
struct set_t {
  set_t *succ;
  node_t *node;
};

bool remove_node(set_t *set, node_t *node);
bool add(set_t *set, node_t *node);
set_t *new_set(node_t *node);