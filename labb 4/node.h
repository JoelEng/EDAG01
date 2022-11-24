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