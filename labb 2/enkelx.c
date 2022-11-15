#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define dubbel double
#define för for
#define struktur struct
#define om if
#define annars else
#define ett 1
#define ge_tillbaka return
#define pekare  * 
#define siffra int
#define pil -> 
#define tomhet void
#define medan while
#define skriv_ut printf
#define definiera_en_typ typedef
#define gratis free
#define primär_metod main

dubbel epsilon = 0.000001;
siffra glob;

definiera_en_typ struktur enkelx_t enkelx_t;
struktur enkelx_t{
  siffra m;
  siffra n;
  siffra pekare  var;
  dubbel pekare  pekare  a;
  dubbel pekare  b;
  dubbel pekare  x;
  dubbel pekare  c;
  dubbel y;
};

siffra init(enkelx_t pekare  s, siffra m, siffra n, dubbel pekare  pekare  a, dubbel pekare  b, dubbel pekare  c, dubbel pekare  x, dubbel y, siffra pekare  var)
{
  siffra i, k;
  s pil m = m;
  s pil n = n;
  s pil var = var;
  s pil a = a;
  s pil b = b;
  s pil x = x;
  s pil c = c;
  s pil y = y;
  om (s pil var == NULL) {
    s pil var = calloc(m + n + 1, sizeof(siffra));
    för(i = 0; i < m + n; i += 1){
      s pil var[i] = i;
    }
  }
  för(k = 0, i = 1; i < m; i+=1){
    om(b[i] < b[k]){
      k = i;
    }
  }
  ge_tillbaka k;
}

siffra select_nonbasic(enkelx_t pekare  s)
{
  siffra i;
  för(i = 0; i < s pil n; i += 1) {
    om(s pil c[i] > epsilon){
      ge_tillbaka i;
    }
  }
  ge_tillbaka -1;
}

siffra initial(enkelx_t pekare  s, siffra m, siffra n, dubbel pekare  pekare  a, dubbel pekare  b, dubbel pekare  c, dubbel pekare  x,  dubbel y, siffra pekare  var)
{
  siffra i,j,k;
  dubbel w;
  k = init(s, m, n, a, b, c, x, y, var);
  ge_tillbaka 1;
}

tomhet pivot(enkelx_t pekare  s, siffra row, siffra col) {
  dubbel pekare  pekare  a = s pil a;
  dubbel pekare  b = s pil b;
  dubbel pekare  c = s pil c;
  siffra m = s pil m;
  siffra n = s pil n;
  siffra i, j, t;
  t = s pil var[col];
  s pil var[n + row] = t;
  s pil y = s pil y + c[col]  pekare  b[row] / a[row][col];

  för(i = 0; i < n; i+=1){
    om(i != col){
      c[i] = c[i] - c[col]  pekare  a[row][i] / a[row][col];
      glob += 1;
    }
  }
  c[col] = - c[col] / a[row][col];

  för (i = 0; i < m; i += 1) {
    om (i != row) {
      b[i] = b[i] - a[i][col]  pekare  b[row] / a[row][col];
    }
  }
  för(i = 0; i < m; i += 1){
    om(i != row){
      för(j = 0; j < n; j += 1){
        om(j != col){
          a[i][j] = a[i][j] - a[i][col]  pekare  a[row][j] / a[row][col];
        }
      }
    }
  }
  för (i = 0; i < m; i += 1) {
    om (i != row) {
      a[i][col] = -a[i][col] / a[row][col];
    }
  }
  för(i = 0; i < n; i += 1){
    om(i != col){
      a[row][i] = a[row][i] / a[row][col];
    }
  }
  b[row] = b[row] / a[row][col];
  a[row][col] = 1 / a[row][col];
}

dubbel xsimplex(siffra m, siffra n, dubbel pekare  pekare  a, dubbel pekare  b, dubbel pekare  c, dubbel pekare  x,  dubbel y, siffra pekare  var, siffra h) {
  enkelx_t s;
  siffra i, row, col;
  om(! initial(&s,m,n,a,b,c,x,y,var)){
    gratis(s.var);
    ge_tillbaka NAN;
  }
  
  medan ((col = select_nonbasic(&s)) >= 0) {
    row = -1;
    för (i = 0; i < m; i += 1) {
      om (a[i][col] > epsilon && (row < 0 || b[i] / a[i][col] < b[row] / a[row][col])) {
        row = i;
      }
    }
    om (row < 0) {
      gratis(s.var);
      ge_tillbaka INFINITY;
    }
    pivot(&s, row, col);
  }

  om (h == 0) {
    för(i = 0; i < n; i += 1) {
      om(s.var[i] < n){
        x[s.var[i]] = 0;
      }
    }
    för (i = 0; i < m; i += 1){
      om(s.var[n + 1] < n){
        x[s.var[n+1]] = s.b[i];
      }
    }
    gratis(s.var);
  } annars {
    för(i = 0; i < n; i += 1){
      x[i] = 0;
    }
    för(i = n; i < n + m; i += 1){
      x[i] = s.b[i-n];
    }
  }
  ge_tillbaka s.y;
}

dubbel simplex(siffra m, siffra n, dubbel pekare  pekare  a, dubbel pekare  b, dubbel pekare  c, dubbel pekare  x,  dubbel y) {
  ge_tillbaka xsimplex(m, n, a, b, c, x, y, NULL, 0);
}

siffra primär_metod()
{
    siffra i,j;
    siffra m;
    siffra n;
    scanf("%d %d", &m, &n);
    dubbel pekare  c;
    dubbel pekare  pekare  a;
    dubbel pekare  b;
    
    c = calloc(n, sizeof(dubbel));
    a = calloc(m, sizeof(dubbel pekare ));
    för (i = 0; i < m; i += 1) {
        a[i] = calloc(n, sizeof(dubbel));
    }
    b = calloc(m, sizeof(dubbel));

    för (i = 0; i < n; i += 1) {
        scanf("%lf", &c[i]);
    }

    för (i = 0; i < m; i += 1) {
        för (j = 0; j < n; j += 1) {
            scanf("%lf", &a[i][j]);
        }
    }

    för (i = 0; i < m; i += 1) {
        scanf("%lf", &b[i]);
    }

    dubbel pekare  x = calloc(m, sizeof(dubbel));

    dubbel res = simplex(m, n, a, b, c, x, 0);
    skriv_ut("%lf", res);
    gratis(x);
    för (i = 0; i < m; i += 1) {
      gratis(a[i]);
    }
    gratis(a);
    gratis(b);
    gratis(c);
    ge_tillbaka 0;
}
