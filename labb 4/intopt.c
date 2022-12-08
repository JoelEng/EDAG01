#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double epsilon = 0.000001;

typedef struct node_t node_t;
struct node_t {
  int m, n, k, h;
  double xh, ak, bk;
  double *min;
  double *max;
  double **a;
  double *b;
  double *x;
  double *c;
  double z;
};

typedef struct set_t set_t;
struct set_t {
  set_t *succ;
  node_t *node;
};

typedef struct simplex_t simplex_t;
struct simplex_t {
  int m;
  int n;
  int *var;
  double **a;
  double *b;
  double *x;
  double *c;
  double y;
};

void free_node(node_t *node) {
  free(node->min);
  free(node->max);
  free(node->b);
  free(node->x);
  free(node->c);
  for (int i = 0; i < node->m + 1; i++) {
    free(node->a[i]);
  }
  free(node->a);
  free(node);
}

bool isempty(set_t *h) {
  while (h != NULL) {
    if (h->node != NULL) {
      return false;
    }
    h = h->succ;
  }
  return true;
}

set_t *new_set(node_t *node) {
  set_t *set = calloc(1, sizeof(set_t));

  set->succ = NULL;
  set->node = node;

  return set;
}

bool add(set_t *set, node_t *node) {
  while (set->succ != NULL) {
    set = set->succ;
  }
  set->succ = new_set(node);
  return true;
}

bool remove_node(set_t *set, node_t *node) {
  node_t *temp;
  while (set != NULL) {
    if (set->node == node) {
      temp = set->node;
      set->node = NULL;
      free_node(temp);
      return true;
    }
    set = set->succ;
  }
  return false;
}

node_t *pop(set_t *set) {
  while (set->node == NULL) {
    set = set->succ;
  }
  node_t *node = set->node;
  set->node = NULL;
  return node;
}

node_t *initial_node(int m, int n, double **a, double *b, double *c) {
  node_t *p = calloc(1, sizeof(node_t));
  int i, j;
  p->a = calloc(m + 1, sizeof(double *));
  for (i = 0; i < m + 1; i += 1) {
    p->a[i] = calloc(n + 1, sizeof(double));
  }
  p->b = calloc(m + 1, sizeof(double));
  p->c = calloc(n + 1, sizeof(double));
  p->x = calloc(n + 1, sizeof(double));
  p->min = calloc(n, sizeof(double));
  p->max = calloc(n, sizeof(double));
  p->m = m;
  p->n = n;

  for (i = 0; i < m; i += 1) {
    for (j = 0; j < n; j += 1) {
      p->a[i][j] = a[i][j];
    }
  }
  memcpy(p->b, b, sizeof(double) * m); // kanske onödigt
  memcpy(p->c, c, sizeof(double) * n);

  for (i = 0; i < n; i += 1) {
    p->min[i] = -INFINITY;
    p->max[i] = INFINITY;
  }
  return p;
}

node_t *extend(node_t *p, int m, int n, double **a, double *b, double *c, int k,
               double ak, double bk) {
  node_t *q = calloc(1, sizeof(node_t));
  int i, j;
  q->k = k;
  q->ak = ak;
  q->bk = bk;

  if (ak > 0 && p->max[k] < INFINITY) {
    q->m = p->m;
  } else if (ak < 0 && p->min[k] > 0) {
    q->m = p->m;
  } else {
    q->m = p->m + 1;
  }

  q->n = p->n;
  q->h = -1;
  q->a = calloc(q->m + 1, sizeof(double *));
  for (i = 0; i < q->m + 1; i += 1) {
    q->a[i] = calloc(q->n + 1, sizeof(double));
  }
  q->b = calloc(q->m + 1, sizeof(double));
  q->c = calloc(q->n + 1, sizeof(double));
  q->x = calloc(q->n + 1, sizeof(double));
  q->min = calloc(n, sizeof(double));
  q->max = calloc(n, sizeof(double));

  for (i = 0; i < p->n; i += 1) {
    q->min[i] = p->min[i];
    q->max[i] = p->max[i];
  }
  for (i = 0; i < m; i += 1) {
    for (j = 0; j < n; j += 1) {
      q->a[i][j] = a[i][j];
    }
    q->b[i] = b[i];
  }
  for (i = 0; i < n; i += 1) {
    q->c[i] = c[i];
  }

  if (ak > 0) {
    if (q->max[k] == INFINITY || bk < q->max[k]) {
      q->max[k] = bk;
    }
  } else if (q->min[k] == -INFINITY || -bk > q->min[k]) {
    q->min[k] = -bk;
  }

  for (i = m, j = 0; j < n; j += 1) {
    if (q->min[j] > -INFINITY) {
      q->a[i][j] = -1;
      q->b[i] = -q->min[j];
      i += 1;
    }
    if (q->max[j] < INFINITY) {
      q->a[i][j] = 1;
      q->b[i] = q->max[j];
      i += 1;
    }
  }
  return q;
}

int is_integer(double *xp) {
  // xp is a pointer to a double

  double x = *xp;
  double r = lround(x); // ISO C lround
  if (fabs(r - x) < epsilon) {
    *xp = r;
    return 1;
  } else {
    return 0;
  }
}

int integer(node_t *p) {
  int i;
  for (i = 0; i < p->n; i += 1) {
    if (!is_integer(&p->x[i])) {
      return 0;
    }
  }
  return 1;
}

void bound(node_t *p, set_t *h, double *zp, double *x) {
  // zp is a pointer to max z found so far
  if (p == NULL) {
    return;
  }

  int i;
  if (p->z > *zp) {
    *zp = p->z;
    // for (i = 0; i < p.n; i += 1) {
    //   x[i] = p.x[i];
    // }
    memcpy(x, p->x, sizeof(double) * p->n); // kanske onödigt

    set_t *current = h;
    while (current->node != NULL) {
      if (current->node->z < p->z) {
        remove_node(h, current->node); // h borde gå att byta ut mot
                                       // current, men vi vågar inte
      }
      if (current->succ != NULL) {
        set_t *temp = current;
        current = current->succ;
      } else {
        return;
      }
    }
  }
}

int branch(node_t *q, double z) {
  double min, max;
  if (q->z < z) {
    return 0;
  }

  int h, i;

  for (h = 0; h < q->n; h += 1) {
    if (!is_integer(&q->x[h])) {
      if (q->min[h] == -INFINITY) {
        min = 0;
      } else {
        min = q->min[h];
      }
      max = q->max[h];

      if (floor(q->x[h]) < min || ceil(q->x[h]) > max) {
        continue;
      }

      q->h = h;
      q->xh = q->x[h];

      return 1;
    }
  }
  return 0;
}
double simplex(int m, int n, double **a, double *b, double *c, double *x,
               double y);

void succ(node_t *p, set_t *h, int m, int n, double **a, double *b, double *c,
          int k, double ak, double bk, double *zp, double *x) {
  node_t *q = extend(p, m, n, a, b, c, k, ak, bk);

  if (q == NULL) { // tog bort &?
    return;
  }
  q->z = simplex(q->m, q->n, q->a, q->b, q->c, q->x, 0);
  if (isfinite(q->z)) {
    if (integer(q)) {
      bound(q, h, zp, x);
    } else if (branch(q, *zp)) {
      add(h, q);
      return;
    }
  }
  free_node(q);
}

double intopt(int m, int n, double **a, double *b, double *c, double *x) {
  node_t *p = initial_node(m, n, a, b, c);

  set_t *h = new_set(p);

  double z = -INFINITY; // best integer solution found so far
  p->z = simplex(p->m, p->n, p->a, p->b, p->c, p->x, 0);
  if (integer(p) || !isfinite(p->z)) {
    z = p->z;
    if (integer(p)) {
      memcpy(x, p->x, sizeof(double) * p->n); // onödigt?
      // delete p
      // free(p);
    }

    while (h != NULL) {
      if (h->node != NULL) {
        free_node(h->node);
      }
      set_t *temp = h->succ;
      free(h);
      h = temp;
    }

    return z;
  }
  branch(p, z);
  while (!isempty(h)) {
    p = pop(h);
    succ(p, h, m, n, a, b, c, p->h, 1, floor(p->xh), &z, x);
    succ(p, h, m, n, a, b, c, p->h, -1, -ceil(p->xh), &z, x);
    free_node(p);
  }

  while (h != NULL) {
    if (h->node != NULL) {
      free_node(h->node);
    }
    set_t *temp = h->succ;
    free(h);
    h = temp;
  }
  // free(h->succ);

  if (z == -INFINITY) {
    return NAN;
  } else {
    return z;
  }
}

// ALLT NEDANFÖR ÄR SIMPLEX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

int initial(simplex_t *s, int m, int n, double **a, double *b, double *c,
            double *x, double y, int *var);

int init(simplex_t *s, int m, int n, double **a, double *b, double *c,
         double *x, double y, int *var) {
  int i, k;
  s->m = m;
  s->n = n;
  s->var = var;
  s->a = a;
  s->b = b;
  s->x = x;
  s->c = c;
  s->y = y;

  if (s->var == NULL) {
    s->var = calloc(m + n + 1, sizeof(int));
    for (i = 0; i < m + n; i += 1) {
      s->var[i] = i;
    }
  }
  for (k = 0, i = 1; i < m; i += 1) {
    if (b[i] < b[k]) {
      k = i;
    }
  }
  return k;
}

int select_nonbasic(simplex_t *s) {
  int i;
  for (i = 0; i < s->n; i += 1) {
    if (s->c[i] > epsilon) {
      return i;
    }
  }
  return -1;
}

void pivot(simplex_t *s, int row, int col) {
  double **a = s->a;
  double *b = s->b;
  double *c = s->c;
  int m = s->m;
  int n = s->n;
  int i, j, t;
  t = s->var[col];
  s->var[col] = s->var[n + row];
  s->var[n + row] = t;
  s->y = s->y + c[col] * b[row] / a[row][col];

  for (i = 0; i < n; i += 1) {
    if (i != col) {
      c[i] = c[i] - c[col] * a[row][i] / a[row][col];
    }
  }
  c[col] = -c[col] / a[row][col];

  for (i = 0; i < m; i += 1) {
    if (i != row) {
      b[i] = b[i] - a[i][col] * b[row] / a[row][col];
    }
  }
  for (i = 0; i < m; i += 1) {
    if (i != row) {
      for (j = 0; j < n; j += 1) {
        if (j != col) {
          a[i][j] = a[i][j] - a[i][col] * a[row][j] / a[row][col];
        }
      }
    }
  }
  for (i = 0; i < m; i += 1) {
    if (i != row) {
      a[i][col] = -a[i][col] / a[row][col];
    }
  }
  for (i = 0; i < n; i += 1) {
    if (i != col) {
      a[row][i] = a[row][i] / a[row][col];
    }
  }
  b[row] = b[row] / a[row][col];
  a[row][col] = 1 / a[row][col];
}

void prepare(simplex_t *s, int k) {
  int m = s->m;
  int n = s->n;
  int i;

  for (i = m + n; i > n; i -= 1) {
    s->var[i] = s->var[i - 1];
  }
  s->var[n] = m + n;
  n += 1;

  for (i = 0; i < m; i += 1) {
    s->a[i][n - 1] = -1;
  }
  s->x = calloc(m + n, sizeof(double));
  s->c = calloc(n, sizeof(double));
  s->c[n - 1] = -1;
  s->n = n;
  pivot(s, k, n - 1);
}

double xsimplex(int m, int n, double **a, double *b, double *c, double *x,
                double y, int *var, int h) {
  simplex_t s;
  int i, row, col;
  if (!initial(&s, m, n, a, b, c, x, y, var)) {
    free(s.var);
    return NAN;
  }

  while ((col = select_nonbasic(&s)) >= 0) {
    row = -1;
    for (i = 0; i < m; i += 1) {
      if (a[i][col] > epsilon &&
          (row < 0 || b[i] / a[i][col] < b[row] / a[row][col])) {
        row = i;
      }
    }
    if (row < 0) {
      free(s.var);
      return INFINITY;
    }
    pivot(&s, row, col);
  }

  if (h == 0) {
    for (i = 0; i < n; i += 1) {
      if (s.var[i] < n) {
        x[s.var[i]] = 0;
      }
    }
    for (i = 0; i < m; i += 1) {
      if (s.var[n + i] < n) {
        x[s.var[n + i]] = s.b[i];
      }
    }
    free(s.var);
  } else {
    for (i = 0; i < n; i += 1) {
      x[i] = 0;
    }
    for (i = n; i < n + m; i += 1) {
      x[i] = s.b[i - n];
    }
  }
  return s.y;
}

int initial(simplex_t *s, int m, int n, double **a, double *b, double *c,
            double *x, double y, int *var) {
  int i, j, k;
  double w;
  k = init(s, m, n, a, b, c, x, y, var);
  if (s->b[k] >= 0) {
    return 1;
  }
  prepare(s, k);
  n = s->n;
  s->y = xsimplex(m, n, s->a, s->b, s->c, s->x, 0, s->var, 1);

  for (i = 0; i < m + n; i += 1) {
    if (s->var[i] == m + n - 1) {
      if (fabs(s->x[i]) > epsilon) {
        free(s->x);
        free(s->c);
        return 0; // infeasible
      } else {
        break; // This i will be used on the next page
      }
    }
  }

  if (i >= n) {
    // x n+m is basic. find good nonbasic.
    j = 0;
    for (k = 0; k < n; k += 1) {
      if (fabs(s->a[i - n][k]) > fabs(s->a[i - n][j])) {
        j = k;
      }
    }
    pivot(s, i - n, j);
    i = j;
  }

  if (i < n - 1) {
    // x n+m is nonbasic and not last. swap columns i and n-1
    k = s->var[i];
    s->var[i] = s->var[n - 1];
    s->var[n - 1] = k;
    for (k = 0; k < m; k += 1) {
      w = s->a[k][n - 1];
      s->a[k][n - 1] = s->a[k][i];
      s->a[k][i] = w;
    }
  }

  free(s->c);
  s->c = c;
  s->y = y;
  for (k = n - 1; k < n + m - 1; k += 1) {
    s->var[k] = s->var[k + 1];
  }

  n = --s->n;
  double *t;
  t = calloc(n, sizeof(double));

  for (k = 0; k < n; k += 1) {
    for (j = 0; j < n; j += 1) {
      if (k == s->var[j]) {
        t[j] = t[j] + s->c[k];
        goto next_k;
      }
    }
    for (j = 0; j < m; j += 1) {
      if (s->var[n + j] == k) {
        break;
      }
    }
    s->y = s->y + s->c[k] * s->b[j];
    for (i = 0; i < n; i += 1) {
      t[i] = t[i] - s->c[k] * s->a[j][i];
    }
  next_k:;
  }
  for (i = 0; i < n; i += 1) {
    s->c[i] = t[i];
  }
  free(t);
  free(s->x);
  return 1;
}

double simplex(int m, int n, double **a, double *b, double *c, double *x,
               double y) {
  return xsimplex(m, n, a, b, c, x, y, NULL, 0);
}

int main2() {
  int i, j;
  int m;
  int n;
  scanf("%d %d", &m, &n);
  double *c;
  double **a;
  double *b;

  c = calloc(n, sizeof(double));
  a = calloc(m, sizeof(double *));
  for (i = 0; i < m; i += 1) {
    a[i] = calloc(n + 1, sizeof(double));
  }
  b = calloc(m, sizeof(double));

  for (i = 0; i < n; i += 1) {
    scanf("%lf", &c[i]);
  }

  for (i = 0; i < m; i += 1) {
    for (j = 0; j < n; j += 1) {
      scanf("%lf", &a[i][j]);
    }
  }

  for (i = 0; i < m; i += 1) {
    scanf("%lf", &b[i]);
  }

  double *x = calloc(m, sizeof(double));
  double res = intopt(m, n, a, b, c, x);

  printf("ANSWER IS: %lf \n", res);
  free(x);
  for (i = 0; i < m; i += 1) {
    free(a[i]);
  }
  free(a);
  free(b);
  free(c);
  return 0;
}