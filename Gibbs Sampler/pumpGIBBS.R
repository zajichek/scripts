#Author: Alex Zajichek
#Bayesian Statistics, Fall 2016

#Gibbs sampler implementation for a Gamma-Poisson hierarchical model
#The Metropolis-Hastings algorithm was also implemented to sample from the 
	#full-conditional distribution of the shape (alpha) parameter of the Gamma distribution
#Full-conditional distributions were previously derived analytically

#Input data and initializing parameter values
x <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t <- c(94.3, 15.7, 62.9, 126, 5.24, 31.4,1.05, 1.05, 2.1, 10.5)
alpha_init <- 10
beta_init <- 10
l_1 <- 1
l_2 <- 1

#For each iteration of the Gibbs sampler, the ‘get_alpha’ function implements 
	#Metropolis-Hastings to obtain a current value of alpha
get_alpha = function(a,b,t,l) {
  
  f <- function(alpha, beta, theta, lambda) {
    n = length(theta)
    (beta^(n*alpha)/gamma(alpha)^n)*exp(-lambda*alpha)*prod(theta^alpha)
  }
  
  cur_value = function(a_old,beta,theta,lambda) {
    a_new = rgamma(1,a_old,1)
    t1 = dgamma(a_old,a_new,1)/dgamma(a_new,a_old,1)
    t2 = f(a_new,beta,theta,lambda)/f(a_old,beta,theta,lambda)
    p = min(c(1,t1*t2))
    choice = sample(c(a_new,a_old),1,prob=c(p,1-p))
    return(choice)
  }
  
  mh = array(NA,1000)
  mh[1] = .1
  for(i in 2:length(mh)) {
    mh[i] = cur_value(mh[i-1],b,t,l)
  }
  return(sample(mh[2:1000],1))
}


#The ‘pump’ function carries out the Gibbs sampling, using ‘get_alpha’ within
pump <- function(x,t,alpha_init,beta_init,iteration,l_1,l_2) {
  n = length(x)
  alpha = array(NA,iteration)
  cur_alpha = alpha_init
  beta = array(NA,iteration)
  cur_beta = beta_init
  theta = matrix(NA,ncol=n,nrow=iteration)
  cur_theta = rgamma(n,shape = x+cur_alpha,scale=1/(t+cur_beta))
  for(i in 1:iteration) {
    print(i)
    cur_beta = rgamma(1,shape = n*cur_alpha + 1, scale = 1/(l_2 + sum(cur_theta)))
    beta[i] = cur_beta
    cur_alpha = get_alpha(cur_alpha, cur_beta, cur_theta,l_1)
    alpha[i] = cur_alpha
    cur_theta = rgamma(n,shape = x+cur_alpha,scale=1/(t+cur_beta))
    theta[i,] = cur_theta
  }
  res = list("Alpha" = alpha, "Beta" = beta, "Theta" = theta)
}

#Run-time was calculated for comparison purposes
start_time <- Sys.time()

#Three chains were used to compare convergence 
chain1 = pump(x,t,10,10,10000,1,1)
chain2 = pump(x,t,5,20,10000,1,1)
chain3 = pump(x,t,30,1,10000,1,1)
end_time <- Sys.time()
elapsed <- end_time - start_time
