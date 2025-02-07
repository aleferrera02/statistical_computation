err_cv<-function(data,h,p,K){
  
  n_max=length(data[,1])
  err_cv=0
  
  #index for the cv
  pos=floor(seq(from=1,to=n_max,length.out=K+1))
  
  #compute the cv error
  for (i in 1:(length(pos)-1)){
    
    x<-data[pos[i]:pos[i+1],1]
    y <- data[pos[i]:pos[i + 1],2][order(data[pos[i]:pos[i + 1],1])]
    f<-locpol(rspnbmd~age,data=data[-(pos[i]:pos[i+1]),],deg=p,bw=h,kernel=EpaK,xeval=x)
    
    err_cv<-err_cv+mean((y-f$lpFit$rspnbmd)^2)
    
  }
  err_cv<-err_cv/K
}

err_cv_mod<-function(data,h,p,K){
  
  err_cv=0
  n_max=length(data[,1])
  list <- vector("list", K)
  len=n_max%/%K
  
  #order the dataset by age
  data<-data[order(data[,1]),]
  
  #list of group of index
  for (i in 1:len){
    if (i<=n_max%%K)
      list[[i]]=seq(i,(len+1)*K,by=K)
    else list[[i]]=seq(i,len*K,by=K)
  } 
  
  #compute the cv error
  for (i in 1:K){
    x<-data[list[[i]],1]
    y <- data[list[[i]],2]
    f<-locpol(rspnbmd~age,data=data[-list[[i]],],deg=p,bw=h,kernel=EpaK,xeval=x)
    
    err_cv<-err_cv+mean((y-f$lpFit$rspnbmd)^2)
    
  }
  err_cv<-err_cv/K
}





