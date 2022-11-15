#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double epsilon = 0.000001;
int glob;

typedef struct enkelx_t enkelx_t;
struct enkelx_t{
  int m;
  int n;
  int* var;
  double** a;
  double* b;
  double* x;
  double* c;
  double y;
};

int init(enkelx_t* s, int m, int n, double** a, double* b, double* c, double* x, double y, int* var)
{
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
    for(i = 0; i < m + n; i += 1){
      s->var[i] = i;
    }
  }
  for(k = 0, i = 1; i < m; i+=1){
    if(b[i] < b[k]){
      k = i;
    }
  }
  return k;
}

int select_nonbasic(enkelx_t* s)
{
  int i;
  for(i = 0; i < s->n; i += 1) {
    if(s->c[i] > epsilon){
      return i;
    }
  }
  return -1;
}

int initial(enkelx_t* s, int m, int n, double** a, double* b, double* c, double* x,  double y, int* var)
{
  int i,j,k;
  double w;
  k = init(s, m, n, a, b, c, x, y, var);
  return 1;
}

void pivot(enkelx_t* s, int row, int col) {
  double** a = s->a;
  double* b = s->b;
  double* c = s->c;
  int m = s->m;
  int n = s->n;
  int i, j, t;
  t = s->var[col];
  s->var[n + row] = t;
  s->y = s->y + c[col] * b[row] / a[row][col];

  for(i = 0; i < n; i+=1){
    if(i != col){
      c[i] = c[i] - c[col] * a[row][i] / a[row][col];
      glob += 1;
    }
  }
  c[col] = - c[col] / a[row][col];

  for (i = 0; i < m; i += 1) {
    if (i != row) {
      b[i] = b[i] - a[i][col] * b[row] / a[row][col];
    }
  }
  for(i = 0; i < m; i += 1){
    if(i != row){
      for(j = 0; j < n; j += 1){
        if(j != col){
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
  for(i = 0; i < n; i += 1){
    if(i != col){
      a[row][i] = a[row][i] / a[row][col];
    }
  }
  b[row] = b[row] / a[row][col];
  a[row][col] = 1 / a[row][col];
}

double xsimplex(int m, int n, double** a, double* b, double* c, double* x,  double y, int* var, int h) {
  enkelx_t s;
  int i, row, col;
  if(! initial(&s,m,n,a,b,c,x,y,var)){
    free(s.var);
    return NAN;
  }
  
  while ((col = select_nonbasic(&s)) >= 0) {
    row = -1;
    for (i = 0; i < m; i += 1) {
      if (a[i][col] > epsilon && (row < 0 || b[i] / a[i][col] < b[row] / a[row][col])) {
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
    for(i = 0; i < n; i += 1) {
      if(s.var[i] < n){
        x[s.var[i]] = 0;
      }
    }
    for (i = 0; i < m; i += 1){
      if(s.var[n + 1] < n){
        x[s.var[n+1]] = s.b[i];
      }
    }
    free(s.var);
  } else {
    for(i = 0; i < n; i += 1){
      x[i] = 0;
    }
    for(i = n; i < n + m; i += 1){
      x[i] = s.b[i-n];
    }
  }
  return s.y;
}

double simplex(int m, int n, double** a, double* b, double* c, double* x,  double y) {
  return xsimplex(m, n, a, b, c, x, y, NULL, 0);
}

int main()
{
    int i,j;
    int m;
    int n;
    scanf("%d %d", &m, &n);
    double* c;
    double** a;
    double* b;
    
    c = calloc(n, sizeof(double));
    a = calloc(m, sizeof(double*));
    for (i = 0; i < m; i += 1) {
        a[i] = calloc(n, sizeof(double));
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

    double* x = calloc(m, sizeof(double));

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
