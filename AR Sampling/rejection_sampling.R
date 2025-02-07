rejection_sampling<- function (n, f, g, rand_g, M) {
  
  samples <- numeric(n)
  i <- 1
  yy <- rand_g(4*n)
  
  time <- Sys.time()
  count <- 0
  
  while (i <= n) {
    
    count <- count + 1
    
    u <- runif(1, 0, 1)
    y <- yy[count]
    
    if (u < f(y)/(M*g(y))) {
      samples[i] <- y
      i <- i + 1
    }
  }
  
  rej_rate <- (count-n)/count
  
  time <- Sys.time() - time
  
  x<-list(samples=samples,time=time,rej_rate=rej_rate)
  
  return(x)
}