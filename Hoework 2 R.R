library(ggplot2)

# QUestion 1
# b) 
a <- c(1.77, -0.23, 2.76, 3.80, 3.47, 56.75, -1.34, 4.24, -2.44,
       3.29, 3.71, -2.40, 4.53, -0.07, -1.05, -13.87, -2.53, -1.75)
n=length(a)
# write a function to calculate the loglikelihood
l_theta  <- function(theta,a)

 {
  
  
 loglike <- -length(a)*log(pi)-sum(log(1+(theta-a)^2))  # calculate the logliklehood for theta=1
 return(loglike)

 }


l_theta(1,a)

#plot the likelihood function for the range of theta from -100 to 100
library(ggplot2)
theta <- c(-100:100)
loglike <- sapply(theta, function(x) l_theta(x, a))
datafram <- data.frame(cbind(theta,loglike))
 ggplot(datafram, aes(x=theta, y=loglike))+ geom_line(aes(y=loglike, x=theta))
 
 ## use Newton method to estimate MLE of theta
 
 l_theta  <- function(theta,a){  # -l
   
    y1 <- length(a)*log(pi)+sum(log(1+(theta-a)^2))
 
   
 }
 
 grad <- function(theta,a){
   
   y2 <- 2*sum((theta-a)/(1+(theta-a)^2))  # - l'

 }
 
  hessi <- function(theta,a){
    
   y3 <- matrix(2*sum((1-(theta-a)^2)/(1+(theta-a)^2)^2)) # -l''

  }
  
  
  
  a <- a
   
  
   theta0 <- c(-11,-1,0,1.5,4,4.7,7,8,38)  # specify the starting vector
   
   result <- sapply(theta0, function(x) nlminb(x,l_theta,grad, hessi, a = a)$par)
     # if it does not return a result, it usually mean your program l, l', l'' wrong
   
   
# c)
 # define the fixed point version of hession matrix
 hessi_fix <- function(theta,a){
   
   return(matrix(1/al,nrow=1))   # it is importan to use return, if use assign '<-' hit enter won't gives you anything
   
 }
 

 
 al=1
 theta0 <- c(-11,-1,0,1.5,4,4.7,7,8,38)  # specify the starting vector
 result1 <- sapply(theta0, function(x) nlminb(x,l_theta,grad, hessi_fix, a = a)$par)

 al=0.64
 theta0 <- c(-11,-1,0,1.5,4,4.7,7,8,38)  # specify the starting vector
 result2 <- sapply(theta0, function(x) nlminb(x,l_theta,grad, hessi_fix, a = a)$par)

 al=0.25
 theta0 <- c(-11,-1,0,1.5,4,4.7,7,8,38)  # specify the starting vector
 result3 <- sapply(theta0, function(x) nlminb(x,l_theta,grad, hessi_fix, a = a)$par)
 
 alfa=c(1,0.64,0.25)
 mytab <- cbind(result1, result2, result3)
 dimnames(mytab) <- list(theta0, alfa)
 knitr:: kable(mytab, digits=8, caption = 'fixed point algorithm')
 
 
 # d)
 # define the fixed point version of hession matrix
 fs <- function(theta,a){               # fisher information
   
   return(matrix(n/2,nrow=1))   
   
 }
 
 
 result_fisher <- sapply(theta0, function(x) nlminb(x,l_theta,grad, fs, a = a)$par) # first use fisher 
 result_fn <- sapply(result_fisher, function(x) nlminb(x,l_theta,grad, fs, a = a)$par) # then use newton to refine
 
 
 #### Q2
 
 #a) graph the loglikelyhood function
 ab <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96,
        2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52)
 n2=length(ab)
 # write a function to calculate the loglikelihood
 l_q2 <- function(theta2,ab){

   return(-n2*log(2*pi)+sum(log(1-(cos(ab-theta2))))) # calculate the logliklehood for theta=1
 }
 
 l_q2(1,ab)
 
 
 #plot the likelihood function for the range of theta from -100 to 100
 
 theta2 <- seq(from= -pi, to = pi, by = 0.05)
 l2 <- sapply(theta2, function(x) l_q2(x, ab))
 datafram2 <- data.frame(cbind(theta2,l2))
 ggplot(datafram2, aes(x=theta2, y=l2))+ geom_line(aes(x=theta2, y=l2)) 
 
 #b) use method of moments to find estimates of theta
 
 theta20 = asin(mean(ab)-pi)

 #c) use newton's method
 
 l2  <- function(theta2,ab){  # -l2
   
   return(n2*log(2*pi)-sum(log(1-(cos(ab-theta2)))))
   
   
 }
 
 grad2 <- function(theta2,ab){
   
   return(-sum(sin(theta2-ab)/(1-cos(theta2-ab))))  # - l'
   
 }
 
 hessi2 <- function(theta2,ab){
   
   return(matrix(-sum(1/(cos(theta2-ab)-1)),nrow=1)) # -l''
   
 }
 
 
  
  result5 <- nlminb(start=theta20,l2,grad2, hessi2, ab = ab)$par
 
 #d)
  nlminb(start=-2.7,l2,grad2, hessi2, ab = ab)$par
  nlminb(start=2.7,l2,grad2, hessi2, ab = ab)$par
#e)
  theta2e=seq(-pi,pi,by = 2*pi/199)
  result6 <- sapply(theta2e, function(x) nlminb(x,l2,grad2, hessi2, ab = ab)$par)
  
  dataframe2e <- cbind(theta2e,result6)
  
  install.packages("dplyr")
  library(dplyr)  # data manipulating package
  dataframe2e <- tbl_df(dataframe2e)  # need to convert data to tbl before use this package
  planes <- group_by(dataframe2e, result6)
  bygroup <- data.frame(planes)
  

  
  ## Q3
  
  #a)
  beetles <- data.frame(
    days = c(0, 8, 28, 41, 63, 69, 97, 117, 135, 154),
    beetles = c(2, 47, 192, 256, 768, 896, 1120, 896, 1184, 1024))
    N <- beetles$beetles
    t <- beetles$days
# gradian function w.r.t K
  grad_k <- function(k,r,t) {
    
    return(   (2*(2+(k-2)*exp(-r*t))-2*k*exp(-r*t)) / ((2+(k-2)*exp(-r*t))^2)      )
  }
  
  grad_r<- function(k,r,t) {
    
    return(   (2*k*(k-2)*exp(-r*t)*r) / ((2+(k-2)*exp(-r*t))^2)         )
  }
  
# a function that returns A matrix 
   A <- function(k0,r0,t){
 
   gk <- grad_k(k0,r0,t)
   gr <- grad_r(k0,r0,t)                    # for example, use k=1 and r=0.02 to test
   return( matrix(cbind(gk,gr), ncol=2))
 
  }
 
 A(1,0.02,t)
 # function that returns Z
  Z <- function(k,r,N,t){
 
   return( N - 2*k/(2+(k-2)*exp(-r*t)) )
 }
 
 Z(1,0.02,N,t)
 
 # set initial value
 theta3 <- matrix(c(1000,0.05),nrow=2)
 delta3 <- matrix(c(1,1),nrow=2)
 
#iteration
 while(crossprod(delta3,delta3)>=0.00000000000001){  # is this the standard we use in multivariate optimization?)
   theta3_1 <- theta3 
   A1 <- A(theta3[1,1],theta3[2,1],t)
   Z1 <- Z(theta3[1,1],theta3[2,1],N,t)
   theta3 <- theta3 + solve(t(A1)%*%A1)%*%t(A1)%*%Z1
   delta3 <- theta3 - theta3_1
 }
 
 estimate <- theta3
 print(estimate)
 
##b)
 
 SSE <- matrix(0,100,100,byrow=T)
 for (i in 1:100){
   for (j in 1:100){
     k <- 500 + 10*j
     r <- 0 + 0.01*i
     SSE[j,i] <- sum(Z(k,r,N,t)^2)
   }
 }
 
 contour(SSE)
 
 ##c) will try to use 'optim'
 n3=length(N)
 ## objective funtion
 
 lognorm <- function(x,N,t){
   
   return(   n3*log(sqrt(2*pi)*x[3])+ sum( (log(N)-log(2*x[1]/(2+(x[1]-2)*exp(-x[2]*t))))^2/ 2* x[3]^2 )    )
   
 }
 
 
 x30 <- c(700,0.09,4)
 
 optim(x30, lognorm, N=n,t=t)$par # MLE estimates for k, r, and sigma
 warnings()
 optimHess(x30, lognorm, N=n,t=t) # observed fisher information
 
 
 
 # calculate the variance of MLE
 