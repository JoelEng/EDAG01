#include <stdio.h>
#include <stdlib.h>

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

    printf("max Z = ");
    for(i = 0; i < n; i +=1){
        if(i > 0){
            printf("%+10.3lf ",c[i]);
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
    return 0;
}
