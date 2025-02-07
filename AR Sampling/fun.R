p <- function(x) {
  if (x < 0 || x > 1) {
    return(0)
  }
  C <- (9 * pi^2) / (15 * pi + 4) # normalization constant
  f <- (1 - x) * sin(3 * pi * x / 2) + 2 * x * sin(2 * pi * (x - 0.5))
  return(C * f)
}

upper_bound <- function(x) {
  if (x < 0 || x > 1) {
    return(0)
  } 
  else {
    return(1)
  }
}

rand_ub <- function(n) {
  return(runif(n, 0, 1))
}

upper_bound_divided <- function(x) {
  if (x < 0 || x > 1) {
    return(0)
  } 
  if (x < 0.4) {
    return(1/3)
  }
  else {
    return(13/9)
  }
}

rand_ubd <- function(n) {
  
  u <- runif(n, 0, 1)
  y <- numeric(n)
  
  for (i in 1:n) {
    if (u[i] < 2/15) {
      y[i] <- 3*u[i]
    }
    else {
      y[i] <- (u[i]-2/15)*9/13+0.4
    }
  }
  return(y)
  
}