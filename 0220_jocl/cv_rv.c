#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void init_array(int n, unsigned long seed, float *x) {
  int i;
  for (i = 0; i < n; i++) {
    seed = seed*1103515245 + 12345;
    x[i] = seed%19 - 9;
  }
}

void cv_rv(float *cv, float *rv, float *prod, int n) {
  int i, j;
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) {
      prod[i*n+j] = cv[i]*rv[j];
    }
  }
}

#define N 8192 

int main(int argc, char *argv[]) {
  float *cv, *rv, *prod;
  clock_t start, end;

  cv   = malloc(  N*sizeof(float));
  rv   = malloc(  N*sizeof(float));
  prod = malloc(N*N*sizeof(float));

  init_array(N, 1, cv);
  init_array(N, 2, rv);

  start = clock();

  cv_rv(cv, rv, prod, N);

  end = clock();

  printf("It took %.3f seconds.\n", (float)(end - start)/CLOCKS_PER_SEC);

  return 0;
}
