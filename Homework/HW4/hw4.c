#include <R.h>
#include <Rmath.h>

//Question 1.
//-----------
void paretoint(double *xmax, double *c, double *p, double *y) {
  double interval= *xmax / pow(10,6);
  double i = 0.0, j = 0.0;
  double x,temp;
  
  for(x= 0.0; x< *xmax; x = x+interval) {
    i = x;
    j = x + interval;
    temp = ((j-i)/6) * (*p-1) * (pow(*c,*p-1)) * (pow((i+ *c),-*p) + pow((j+ *c),-*p) + 4 *
            pow(((i+j)/2 + *c),-*p));
    *y = *y + temp;
  }
}

//Question 2.
//-----------
void kernreg2 (double *x, double *y, int *n, double *b,double *g2, int *m, double *est)
{
  int i,j;
  double a1,a2,c;
  for(i = 0; i < *m; i++) {
    a1 = 0.0;
    a2 = 0.0;
    for(j=0; j < *n; j++) {
      c=dnorm(x[j]-g2[i],0,*b,0);
      a1  +=  y[j]  *  c;
      a2 += c;
    }
    if(a2 > 0.0)
      est[i] = a1/a2;
    else est[i] = 0.0;
  }
}
