#Metropolis-Hastings algorithm to sample from full-conditional

get_alpha = function(a,b,t,l) {

cur_value = function(a_old,beta,theta,lambda) {
  a_new = rnorm(1,a_old,.1)
  t1 = dnorm(a_old,a_new,.1)/dnorm(a_new,a_old,.1)
  t2 = f(a_new,beta,theta,lambda)/f(a_old,beta,theta,lambda)
  p = min(c(1,t1*t2))
  choice = sample(c(a_new,a_old),1,prob=c(p,1-p))
  return(choice)
}

f = function(alpha, beta, theta, lambda) {
  n = length(theta)
  (beta^(n*alpha)/gamma(alpha)^n)*exp(-lambda*alpha)*prod(theta^alpha)
}

mh = array(NA,10000)
mh[1] = a
for(i in 2:length(mh)) {
  mh[i] = cur_value(mh[i-1],b,t,l)
}
return(sample(mh,1))
}
