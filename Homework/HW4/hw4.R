# Question 1
#===========
system("/usr/local/bin/R CMD SHLIB hw4.c")
dyn.load("hw4.so")

paretoint2 <- function(xmax, c, p){
  .C("paretoint",as.double(xmax), as.double(c), as.double(p), y=as.double(0))
}

xmaxvalues = c(10, 25, 50, 75, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
y = c()

for(i in 1:length(xmaxvalues)) {
  y = append(y, paretoint2(xmaxvalues[i], 3.5, 2.5)$y); 
}

plot(xmaxvalues, y, xlab="xmax", ylab="paretoint", main="paretoint(xmax,3.5,2.5) vs. xmax", pch=16,col="black");

y = c()
for(i in 1:length(xmaxvalues)) {
  y = append(y, paretoint2(xmaxvalues[i], 11.3, 3.2)$y); 
}

plot(xmaxvalues, y, xlab="xmax", ylab="paretoint", main="paretoint(xmax,11.3,3.2) vs. xmax", pch=16,col="red");


# Question 2
#===========
# Part a)
system("/usr/local/bin/R CMD SHLIB hw4.c")
dyn.load("hw4.so")

# Part b)
input = scan("x15.txt",what="char") 
z = matrix(input,ncol=6,byrow=T) 
x = as.numeric(z[,2]) 
y = as.numeric(z[,6])

# Part c)
n = 48
bw = bw.nrd(x)
m = 100
g2 = seq(min(x),max(x),length=m)
a3 = .C("kernreg2", as.double(x), as.double(y), as.integer(n), as.double(bw), as.double(g2), as.integer(m), estim=double(m))

# Part d)
xi = rep(0,100);
yi = rep(0,100);
samplexy = sample(1:48, 100, replace=TRUE)

for(j in 1:100)
{
  k = samplexy[j]
  xi[j] = x[k]
  yi[j] = y[k]
}
n = length(xi)
m = 100
a3 = .C("kernreg2", as.double(xi), as.double(yi), as.integer(n), as.double(bw), as.double(g2), as.integer(m), estim=double(m))

# Part e)
data <- data.frame() 
xi=rep(0,100) 
yi=rep(0,100)
for(i in 1:200) {
  samplexy = sample(1:48, 100, replace=TRUE) 
  for(j in 1:100) {
    k = samplexy[j]
    xi[j] = x[k] 
    yi[j] = y[k] 
  }
  n=length(xi)
  m = 100
  a3 = .C("kernreg2", as.double(xi), as.double(yi), as.integer(n), as.double(bw), as.double(g2), as.integer(m), est=double(m))
  data <- rbind(data,a3$est)
}

x25 = rep(0,100)
x975 = rep(0,100)
for(i in 1:100)
{
  xtemp <- data[[i]]
  x25[i]=quantile(xtemp,0.025)
  x975[i]=quantile(xtemp,0.975)
}

# Part f)
plot(g2, a3$est, type="n", xlab="Grid Points", ylab="Kernel Desnsity Estimates",main="Kernel Regression with 95% Confidence Band")
points (g2,a3$est,type="l",col="green")
points(g2,x25,type="l",lty=2,col="red")
points(g2,x975,type="l",lty=2,col="black")

