#' Monte Carlo Central Limit Theorem for Uniform Distribution
#'
#' This function draws repeated samples from a Uniform(a,b) distribution,
#' computes sample means, and plots their histogram along with a kernel density curve
#' and the theoretical normal curve predicted by the Central Limit Theorem.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of repeated samples.
#' @param a Lower bound of the uniform distribution.
#' @param b Upper bound of the uniform distribution.
#'
#' @return A histogram comparing empirical and theoretical sampling distributions.
#' @export
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=1.1*max(param$density)
  hist(w,freq=FALSE,ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n","sample size= ",n,sep=""),
       xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
