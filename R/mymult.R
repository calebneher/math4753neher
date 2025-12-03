#' Multinomial Distribution Simulation
#'
#' This function simulates a multinomial distribution by repeatedly sampling
#' from k categories and creates a barplot of the relative frequencies.
#'
#' @param iter Number of iterations (default = 100)
#' @param n Sample size per iteration (default = 10)
#' @param p Vector of probabilities for each category (default = c(1,1,1,1)/4)
#'
#' @return A matrix containing frequency counts for each category across iterations,
#'   and displays a barplot of relative frequencies
#' @export
#'
#' @examples
#' mymult(iter=1000, n=10, p=c(1,2,3,4,2)/12)
#' mymult(iter=500, n=20, p=c(0.25, 0.25, 0.25, 0.25))
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  k=length(p)
  tab.mat=matrix(NA,nr=k,nc=iter, byrow=TRUE)

  for(i in 1:iter){
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }

  freq=apply(tab.mat,1,sum)
  names(freq)=1:k
  barplot(freq/(n*iter),col=rainbow(k) )
  tab.mat
}
