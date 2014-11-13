#include <R.h>
#include <Rmath.h>

//Question 1:
//===========
void alt2 (int *num, double *output) {
  output[0] = 1.0;
	for(int i=1; i<*num; i++) {
    if(i%2 != 0)
      output[i] = output[i-1] - 1.0/(i+1.0);
    else
      output[i] = output[i-1] + 1.0/(i + 1.0);
	}
}

//Question 2:
//===========
void kDensity (int *m, int *n, double *g, double *x, double *y, double *bw) {
  double a;
  for(int i=0; i<*m; i++) {
    a = 0.0;
    for(int j=0; j<*n; j++) {
      a += dnorm((x[j]-g[i]) / *bw, 0, 1, 0);
    }
    y[i] = a/(*n * *bw);
  }
}