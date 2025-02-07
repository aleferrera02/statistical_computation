mixing_prop<-function(N,mu1,v1,mu2,v2,tau){
    
    ind <- I(runif(N) > tau)
    x <- rep(0,N)
    x[ind] <- rnorm(sum(ind), mu1, sqrt(v1))
    x[!ind] <- rnorm(sum(!ind), mu2, sqrt(v2))
    return(x)
}

f<-function(x,mu1,v1,mu2,v2,tau){
  return(dnorm(x,mu1,sqrt(v1))*(1-tau)+dnorm(x,mu2,sqrt(v2))*tau)
}

