#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

double epsilon = 0.000001;

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
  n = n + 1;

  for (i = 0; i < m; i++) {
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
      if (s.var[n + 1] < n) {
        x[s.var[n + 1]] = s.b[i];
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
  if (b[k] >= 0) {
    return 1;
  }
  prepare(s, k);
  n = s->n;
  s->y = xsimplex(m, n, s->a, s->b, s->c, s->x, 0, s->var, 1);

  for (i = 0; i < m + n; i += 1) {
    if (s->var[i] == m + n - 1) {
      if (abs(s->x[i]) > epsilon) {
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
      if (abs(s->a[i - n][k]) > abs(s->a[i - n][j])) {
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
    for (k = 0; k < m; k = k + 1) {
      w = s->a[k][n - 1];
      s->a[k][n - 1] = s->a[k][i];
      s->a[k][i] = w;
    }
  } else {
    // x n+m is nonbasic and last. forget it
  }

  free(s->c);
  s->c = c;
  s->y = y;
  for (k = n - 1; k < n + m - 1; k += 1) {
    s->var[k] = s->var[k + 1];
  }

  n = s->n - 1;
  s->n = s->n - 1;
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

int main() {
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

  printf("max Z = ");
  for (i = 0; i < n; i += 1) {
    if (i > 0) {
      printf("%+10.3lf ", c[i]);
    } else {
      printf("%10.3lf", c[i]);
    }
  }
  printf("\n");

  for (i = 0; i < m; i += 1) {
    printf("\t");
    for (j = 0; j < n; j += 1) {
      if (j > 0) {
        printf("%+10.3lf", a[i][j]);
      } else {
        printf("%10.3lf", a[i][j]);
      }
    }
    printf(" \u2264 %8.3lf\n", b[i]);
  }

  double res = simplex(m, n, a, b, c, x, 0);
  printf("%lf", res);
  free(x);
  for (i = 0; i < m; i += 1) {
    free(a[i]);
  }
  free(a);
  free(b);
  free(c);
  return 0;
}
