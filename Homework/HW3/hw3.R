setwd("~/Desktop/gitRepos/StatisticalProgramming/Homework/HW3")
#Question 1:
#===========
system("/usr/local/bin/R CMD SHLIB hw3.c")
dyn.load("hw3.so")

alt2 = function(n) {
	.C("alt2", as.integer(n), y = double(n))
}

result = alt2(1000000)
x = 1:1000000

plot(x,result$y,xlab="n",ylab="ln(2)",main="Approximation of ln(2)",col="red" ) 


#Question 2:
#===========
dataE = scan("SearchResults.txt", skip=3, nlines=2395, what="char")
dataMatrixE = matrix(dataE, ncol=12, byrow=T)
colNamesE = scan("SearchResults.txt", skip=2, nlines=1, what="char")
colnames(dataMatrixE) = colNamesE
mag_vec = as.numeric(as.vector(dataMatrixE[,'MAG']))


system("/usr/local/bin/R CMD SHLIB hw3.c")
dyn.load("hw3.so")

kDensity = function(mag_vec) {
  bandwidth=bw.nrd(mag_vec)
  gridPoints = seq(min(mag_vec), max(mag_vec), length=100)
  m = length(gridPoints)
  n = length(mag_vec)
  .C("kDensity", as.integer(m), as.integer(n), g=as.double(gridPoints), 
     as.double(mag_vec), y=double(m), as.double(bandwidth))
}

a = kDensity(mag_vec)
plot(a$g,a$y,type="l",xlab="Earthquake Magnitudes",ylab="Kernel Density Estimates",col="red",main="Kernel density Estimates vs Earthquake Magnitudes")
