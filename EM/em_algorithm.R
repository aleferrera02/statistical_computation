em_algorithm<-function(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps){
  
  count<-0
  flag<-eps+1
  mu1<-mu1_0
  v1<-v1_0
  mu2<-mu2_0
  v2<-v2_0
  tau<-tau_0
  g<-dnorm(x,mu2,sqrt(v2))*tau/f(x,mu1,v1,mu2,v2,tau)
  
  loglike<-sum(log(f(x,mu1,v1,mu2,v2,tau)))
  
  while(flag>eps){
    
    count=count+1
    
    mu1<-sum((1-g)*x)/sum(1-g)
    mu2<-sum(g*x)/sum(g)
    v1<-sum((1-g)*(x-mu1)^2)/sum(1-g)
    v2<-sum(g*(x-mu2)^2)/sum(g)
    tau<-sum(g)/length(x)
    
    loglike_new<-sum(log(f(x,mu1,v1,mu2,v2,tau)))
    flag<-abs(loglike_new-loglike)
    loglike<-loglike_new
    
    g<-dnorm(x,mu2,sqrt(v2))*tau/f(x,mu1,v1,mu2,v2,tau)
    
  }
  
  par<-list(mu1=mu1,v1=v1,mu2=mu2,v2=v2,tau=tau,count=count)
  return(par)
}