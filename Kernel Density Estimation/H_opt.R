#function that computes the optimal h with n,a,b,N fixed

h_opt<-function(n,a,b,N){
  
  X<-rbeta(n,a,b)
  X_sorted<-sort(X)
  Y<-sin((X_sorted/3+0.1)^(-1))+rnorm(n,0,1)
  
  pos<-seq(from=0,to=n,length.out=N+1)
  pos<-round(pos)
  teta_hat<-0
  sigma_hat<-0
  
  for (i in seq(length(pos)-1)){
    x_rest<-X_sorted[(pos[i]+1):pos[i+1]]
    y_rest<-Y[(pos[i]+1):pos[i+1]]
    
    df <- data.frame(
      y = y_rest,
      x1 =  x_rest,
      x2 = x_rest^2,
      x3 = x_rest^3,
      x4 = x_rest^4)
    
    gm<-lm(y ~ .,data=df)
    cf<-as.numeric(gm$coefficients)
    
    min_length <- min(length(y_rest), length(gm$fitted.values))
    
    teta_hat<-teta_hat+sum((2*cf[3]+6*cf[4]*x_rest+12*cf[5]*x_rest^2)^2)
    sigma_hat<-sigma_hat+sum((y_rest[1:min_length]-gm$fitted.values[1:min_length]) ^2)
  }
  
  teta_hat<-teta_hat/n
  sigma_hat<-sigma_hat/(n-5*N)
  h_ott<-n^(-1/5)*(35*sigma_hat*1/teta_hat)^(1/5)
  
}

#function that computes the optimal h against n with a,b,N fixed

hopt_versus_n<-function(n,a,b,N){

  h_ott<-rep(0,length(n))
  
  for (j in seq(length(n))){
    h_ott[j]<-h_opt(n[j],a,b,N)
  }

return(h_ott)
  
}

#function that computes the optimal h against N with n,a,b fixed

hopt_versus_N<-function(n,a,b,N){
  
  h_ott<-rep(0,length(N))
  
  for (j in seq(length(N))){
    h_ott[j]<-h_opt(n,a,b,N[j])
  }
  
  return(h_ott)

}

#function that computes the optimal h against a with n,b,N fixed

hopt_versus_a<-function(n,a,b,N){
  
  h_ott<-rep(0,length(a))
  
  for (j in seq(length(a))){
    h_ott[j]<-h_opt(n,a[j],b,N)
  }
  
  return(h_ott)
  
}

#function that computes the optimal h against b with n,a,N fixed

hopt_versus_b<-function(n,a,b,N){

  h_ott<-rep(0,length(b))
  
  for (j in seq(length(b))){
    h_ott[j]<-h_opt(n,a,b[j],N)
  }
  
  return(h_ott)
  
}
