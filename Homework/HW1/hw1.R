setwd("~/Dropbox/UCLA_Classes/2014_Fall/STATS 202A - Statistical Programming/Homework/HW1")

## Question 1.a
func90 = function (v) {
  v_sorted = sort(v)
  v_90 = v_sorted[90]
  return (v_90)
}

## Question 1.b
func91 = function (v) {
  v_sorted = sort(v)
  v_91 = v_sorted[91]
  return (v_91)
}

## Question 1.c
funcMeanOf90_91 = function (v) {
  v_sorted = sort(v)
  v_meanOf_90_91 = (v_sorted[90] + v_sorted[91])/2
  return (v_meanOf_90_91)
}

## Question 1.d (i)
ud_100 = runif(100)
v_90 = func90(ud_100)
v_91 = func91(ud_100)
v_mean_90_91 = funcMeanOf90_91(ud_100)
quantile_90 = quantile(ud_100, 0.9)

## Question 1.d (ii)
l_90 = c()
l_91 = c()
l_mean_90_91 = c()
l_quantile_90 = c()
for (i in 1:100000) {
  ud_100_t = runif(100)
  l_90[i] = func90(ud_100_t)
  l_91[i] = func91(ud_100_t)
  l_mean_90_91[i] = funcMeanOf90_91(ud_100_t)
  l_quantile_90[i] = quantile(ud_100_t, 0.9)
}

## Question 1.d (iii)
mean_l_90 = cumsum(l_90) / 1:100000
mean_l_91 = cumsum(l_91) / 1:100000
mean_l_mean_90_91 = cumsum(l_mean_90_91) / 1:100000
mean_l_quantile_90 = cumsum(l_quantile_90) / 1:100000

plot(c(0,100000), c(min(mean_l_90),max(mean_l_90)),type="n", xlab="x co-ordinates", ylab="y co-ordinates")
points(mean_l_90,pch=".", col="red")

plot(c(0,100000), c(min(mean_l_91),max(mean_l_91)),type="n", xlab="x co-ordinates", ylab="y co-ordinates")
points(mean_l_91,pch=".", col="blue")

plot(c(0,100000), c(min(mean_l_mean_90_91),max(mean_l_mean_90_91)),type="n", xlab="x co-ordinates", ylab="y co-ordinates")
points(mean_l_mean_90_91,pch=".", col="cyan")

plot(c(0,100000), c(min(mean_l_quantile_90),max(mean_l_quantile_90)),type="n", xlab="x co-ordinates", ylab="y co-ordinates")
points(mean_l_quantile_90,pch=".", col="purple")

## Question 1.e
mean_l_90[100000]
mean_l_91[100000]
mean_l_mean_90_91[100000]
mean_l_quantile_90[100000]
y_min = min(c(min(mean_l_90), min(mean_l_91), min(mean_l_mean_90_91), min(mean_l_quantile_90)))
y_max = max(c(min(mean_l_90), max(mean_l_91), max(mean_l_mean_90_91), max(mean_l_quantile_90)))

plot(c(0,100000), c(y_min,y_max),type="n", xlab="x co-ordinates", ylab="y co-ordinates")
points(mean_l_90,pch=".", col="red")
points(mean_l_91,pch=".", col="blue")
points(mean_l_mean_90_91,pch=".", col="cyan")
points(mean_l_quantile_90,pch=".", col="purple")


## Question 2.a
pi2 = function(n) {
  return(sqrt(6*sum(1/(c(1:n)^2))))
}

pi2(10^0)
pi2(10^1)
pi2(10^2)
pi2(10^3)
pi2(10^4)
pi2(10^5)
pi2(10^6)

## Question 2.b
pi3 = function(n) {
  x = runif(n, -1, 1)
  y = runif(n, -1, 1)
  xin = c()
  xout = c()
  yin = c()
  yout = c()
  pin = 0
  for(i in 1:n) {
    d = (x[i])^2 + (y[i])^2
    if (d <= 1) {
      pin = pin + 1
      xin = c(xin, x[i])
      yin = c(yin, y[i])
    } else {
      xout = c(xout, x[i])
      yout = c(yout, y[i])
    }
  }
  ret = list(pin*4/n, xin, yin, xout, yout)
  return(ret)
}

(pi3(10^0))[[1]]
(pi3(10^1))[[1]]
(pi3(10^2))[[1]]
(pi3(10^3))[[1]]
(pi3(10^4))[[1]]
(pi3(10^5))[[1]]
res_6 = pi3(10^6)
res_6[[1]]

plot(c(-1,1),c(-1,1),type="n",xlab="x-coordinates",ylab="y-coordinates");
points(res_6[[2]],res_6[[3]],pch='.',col="green") ;
points(res_6[[4]],res_6[[5]],pch='.',col="blue");

