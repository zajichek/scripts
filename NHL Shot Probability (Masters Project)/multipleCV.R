#Selecting indices for 10 fold CV
load("ProjectData3.Rda")
nhl = subset(nhl, Minute > 0 & Minute <=65)

errs = function(probs, resp) {
  true = as.numeric(resp == 'GOAL')
  
  #Run for each cut value
  e = function(cut) {
    preds = as.numeric(probs >= cut)
    fp = length(which(preds == 1 & true == 0))/length(which(true == 0))
    fn = length(which(preds == 0 & true == 1))/length(which(true == 1))
    te = length(which((preds == 1 & true == 0) | (preds == 0 & true == 1)))/length(true)
    return(c(te,fp,fn))
  }
  return(sapply(seq(0,1,.01), e))
}


#__________________10-Fold CV for Empirical estimatation, k replications___________________
emp_results = list()
for(k in 1:50) {
inds = 1:579181
cv_inds = list()
for(j in 1:9) {
  temp = sample(inds,57918)
  cv_inds[[j]] = temp
  inds = setdiff(inds, temp)
}
cv_inds[[10]]=inds


empirical_probs = data.frame()
for(i in 1:10) {
  train = nhl[-cv_inds[[i]],]
  test = nhl[cv_inds[[i]],]
  g = subset(train, etype == 'Goal')
  s = subset(train,etype == 'Save')
  
  g_prior = length(g[,1])/(length(g[,1])+length(s[,1]))
  s_prior = 1 - g_prior
  
  g_min = approxfun(density(g$Minute)$x,density(g$Minute)$y,yleft = .00000001, yright = .00000001)
  s_min = approxfun(density(s$Minute)$x,density(s$Minute)$y,yleft = .00000001, yright = .00000001)
  g_shoot = table(g$shooter)/length(g$shooter)
  s_shoot = table(s$shooter)/length(s$shooter)
  g_type = table(g$type)/length(g$type)
  s_type = table(s$type)/length(s$type)
  g_dist = approxfun(density(g$distance)$x, density(g$distance)$y,yleft = .00000001, yright = .00000001)
  s_dist = approxfun(density(s$distance)$x, density(s$distance)$y,yleft = .00000001, yright = .00000001)
  g_home = table(g$home)/length(g$home)
  s_home = table(s$home)/length(s$home)
  g_man = table(g$manpower)/length(g$manpower)
  s_man = table(s$manpower)/length(s$manpower)
  g_play = table(g$playoffs)/length(g$playoffs)
  s_play = table(s$playoffs)/length(s$playoffs)
  g_ang = approxfun(density(g$angles)$x, density(g$angles)$y,yleft = .00000001, yright = .00000001)
  s_ang = approxfun(density(s$angles)$x, density(s$angles)$y,yleft = .00000001, yright = .00000001)
  g_left = table(g$left_shot)/length(g$left_shot)
  s_left = table(s$left_shot)/length(s$left_shot)
  g_height = approxfun(density(g$height)$x, density(g$height)$y,yleft = .00000001, yright = .00000001)
  s_height = approxfun(density(s$height)$x, density(s$height)$y,yleft = .00000001, yright = .00000001)
  g_weight = approxfun(density(g$weight)$x, density(g$weight)$y,yleft = .00000001, yright = .00000001)
  s_weight = approxfun(density(s$weight)$x, density(s$weight)$y,yleft = .00000001, yright = .00000001)
  g_catch = table(g$catch)/length(g$catch)
  s_catch = table(s$catch)/length(s$catch)
  
  
  e_prob = function(obs) {
    top = as.numeric(g_prior)*as.numeric(g_min(obs[[1]]))*as.numeric(g_shoot[obs[[2]]])*as.numeric(g_type[obs[[3]]])*as.numeric(g_dist(obs[[4]]))*as.numeric(g_home[1+obs[[5]]])*as.numeric(g_man[obs[[6]]])*as.numeric(g_play[1+obs[[7]]])*as.numeric(g_ang(obs[[8]]))*as.numeric(g_left[1+obs[[9]]])*as.numeric(g_height(obs[[10]]))*as.numeric(g_weight(obs[[11]]))*as.numeric(g_catch[obs[[12]]]);
    bot = as.numeric(s_prior)*as.numeric(s_min(obs[[1]]))*as.numeric(s_shoot[obs[[2]]])*as.numeric(s_type[obs[[3]]])*as.numeric(s_dist(obs[[4]]))*as.numeric(s_home[1+obs[[5]]])*as.numeric(s_man[obs[[6]]])*as.numeric(s_play[1+obs[[7]]])*as.numeric(s_ang(obs[[8]]))*as.numeric(s_left[1+obs[[9]]])*as.numeric(s_height(obs[[10]]))*as.numeric(s_weight(obs[[11]]))*as.numeric(s_catch[obs[[12]]]);
    return(top/(top+bot))
  }
  
  X = test[,-1]
  probs = array(NA,length(X[,1]))
  for(j in 1:length(X[,1])) {
    probs[j] = e_prob(X[j,])
  }
  
  temp_probs = data.frame("Inds" = cv_inds[[i]], "Pred_probs" = probs, "Truth"=test$etype)
  empirical_probs = rbind(empirical_probs,temp_probs)
}
  blah = errs(empirical_probs$Pred_probs, empirical_probs$Truth)
  emp_results[[k]] = data.frame("Cutoff"= seq(0,1,.01), "Test Error" = blah[1,], "False Positives" = blah[2,], "False Negatives" = blah[3,])
}




#______10 fold CV for parametric model_________________
load("ProjectData3.Rda",verbose = T)
load(file="cv_inds.Rdata")
library(MASS)
nhl = subset(nhl, Minute > 0 & Minute <= 65)

parametric_probs = data.frame()
for(i in 1:10) {
  train = nhl[-cv_inds[[i]],]
  test = nhl[cv_inds[[i]],]
  
  g = subset(train, etype == 'Goal')
  s = subset(train,etype == 'Save')
  
  #Minute-Piecewise Uniform
  reg1 = length(which(g$Minute <= 60))/length(g$Minute)
  overtime1 = 1 - reg1
  reg2 = length(which(s$Minute <= 60))/length(s$Minute)
  overtime2 = 1-reg2
  
  #Distance
  m_g_dist = fitdistr(g$distance, 'gamma')$estimate
  s_g_dist = fitdistr(s$distance, 'gamma')$estimate
  
  #angles
  temp_g = g$angles + 90.021
  temp_s = s$angles + 90.021
  m_g_ang = fitdistr(temp_g, 'weibull')$estimate
  s_g_ang = fitdistr(temp_s, 'weibull')$estimate
  
  #height
  m_g_ht = fitdistr(g$height, 'normal')$estimate
  s_g_ht = fitdistr(s$height, 'normal')$estimate
  
  #weight
  m_g_wt = fitdistr(g$weight, 'normal')$estimate
  s_g_wt = fitdistr(s$weight, 'normal')$estimate
  
  
  #______Getting predicted probabilites of training data
  g_prior = length(g[,1])/(length(g[,1])+length(s[,1]))
  s_prior = 1 - g_prior
  g_shoot = table(g$shooter)/length(g$shooter)
  s_shoot = table(s$shooter)/length(s$shooter)
  g_type = table(g$type)/length(g$type)
  s_type = table(s$type)/length(s$type)
  g_home = table(g$home)/length(g$home)
  s_home = table(s$home)/length(s$home)
  g_man = table(g$manpower)/length(g$manpower)
  s_man = table(s$manpower)/length(s$manpower)
  g_play = table(g$playoffs)/length(g$playoffs)
  s_play = table(s$playoffs)/length(s$playoffs)
  g_left = table(g$left_shot)/length(g$left_shot)
  s_left = table(s$left_shot)/length(s$left_shot)
  g_catch = table(g$catch)/length(g$catch)
  s_catch = table(s$catch)/length(s$catch)
  g_min = function(x) {
    if(x <=60) {
      return(dunif(x,1,60)*reg1)
    } else {
      return(dunif(x,60,65)*overtime1)
    }
  }
  s_min = function(x) {
    if(x <=60) {
      return(dunif(x,1,60)*reg2)
    } else {
      return(dunif(x,60,65)*overtime2)
    }
  }
  g_dist = function(x) {
    return(dgamma(x, shape = m_g_dist[[1]], rate = m_g_dist[[2]]))
  }
  s_dist = function(x) {
    return(dgamma(x, shape = s_g_dist[[1]], rate = s_g_dist[[2]]))
  }
  g_ang = function(x) {
    return(dweibull(x+90.021, shape = m_g_ang[[1]], scale = m_g_ang[[2]]))
  }
  s_ang = function(x) {
    return(dweibull(x+90.021, shape = s_g_ang[[1]], scale = s_g_ang[[2]]))
  }
  g_height = function(x) {
    return(dnorm(x, mean = m_g_ht[[1]], sd = m_g_ht[[2]]))
  }
  s_height = function(x) {
    return(dnorm(x, mean = s_g_ht[[1]], sd = s_g_ht[[2]]))
  }
  g_weight = function(x) {
    return(dnorm(x, mean = m_g_wt[[1]], sd = m_g_wt[[2]]))
  }
  s_weight = function(x) {
    return(dnorm(x, mean = s_g_wt[[1]], sd = s_g_wt[[2]]))
  }
  par_prob = function(obs) {
    top = as.numeric(g_prior)*as.numeric(g_min(obs[[1]]))*as.numeric(g_shoot[obs[[2]]])*as.numeric(g_type[obs[[3]]])*as.numeric(g_dist(obs[[4]]))*as.numeric(g_home[1+obs[[5]]])*as.numeric(g_man[obs[[6]]])*as.numeric(g_play[1+obs[[7]]])*as.numeric(g_ang(obs[[8]]))*as.numeric(g_left[1+obs[[9]]])*as.numeric(g_height(obs[[10]]))*as.numeric(g_weight(obs[[11]]))*as.numeric(g_catch[obs[[12]]]);
    bot = as.numeric(s_prior)*as.numeric(s_min(obs[[1]]))*as.numeric(s_shoot[obs[[2]]])*as.numeric(s_type[obs[[3]]])*as.numeric(s_dist(obs[[4]]))*as.numeric(s_home[1+obs[[5]]])*as.numeric(s_man[obs[[6]]])*as.numeric(s_play[1+obs[[7]]])*as.numeric(s_ang(obs[[8]]))*as.numeric(s_left[1+obs[[9]]])*as.numeric(s_height(obs[[10]]))*as.numeric(s_weight(obs[[11]]))*as.numeric(s_catch[obs[[12]]]);
    return(top/(top+bot))
  }
  
  X = test[,-1]
  probs = array(NA,length(X[,1]))
  for(j in 1:length(X[,1])) {
    probs[j] = par_prob(X[j,])
  }
  
  temp_probs = data.frame("Inds" = cv_inds[[i]], "Pred_probs" = probs, "Truth"=test$etype)
  parametric_probs = rbind(parametric_probs,temp_probs)
}
save(parametric_probs,file="parametric_probs-CV.Rdata")


#__________10 fold CV for Logistic Regression___________
load("ProjectData3.Rda",verbose = T)
nhl = subset(nhl, Minute > 0 & Minute <= 65)
load(file="cv_inds.Rdata")

logistic_probs = data.frame()
for(i in 1:10) {
  train = nhl[-cv_inds[[i]],]
  test = nhl[cv_inds[[i]],]
  
  #Running logistic model
  train$home = factor(train$home);train$playoffs=factor(train$playoffs);train$left_shot=factor(train$left_shot)
  test$home = factor(test$home);test$playoffs=factor(test$playoffs);test$left_shot=factor(test$left_shot)
  model = glm(etype~.,data=train,family = "binomial")
  probs = predict(model,newdata=test,type="response")
  temp_probs = data.frame("Inds" = cv_inds[[i]], "Pred_probs" = probs, "Truth"=test$etype)
  logistic_probs = rbind(logistic_probs,temp_probs)
}
save(logistic_probs,file="logistic_probs-CV.Rdata")
