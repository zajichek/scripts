#Parametric Density Estimation
library(ggplot2)
load("ProjectData3.Rda",verbose = T)
nhl = subset(nhl, Minute > 0 & Minute <= 65)
nhl$etype = factor(nhl$etype)
levels(nhl$etype) = c("Save","Goal")
g = subset(nhl, etype == 'Goal')
s = subset(nhl, etype == 'Save')

#Minute-Piecewise Uniform
reg1 = length(which(g$Minute <= 60))/length(g$Minute)
overtime1 = 1 - reg1
reg2 = length(which(s$Minute <= 60))/length(s$Minute)
overtime2 = 1-reg2
x1 = 1:60;x2 = 60:65
d11 = dunif(x1,1,60)*reg1;d12 = dunif(x2,60,65)*overtime1
d21 = dunif(x1,1,60)*reg2;d22 = dunif(x2,60,65)*overtime2
levels(nhl$etype) = c("Save","Goal")
p1 = ggplot(g,aes(Minute)) + geom_density(aes(fill=I("green"),colour = I("green"),alpha=.5),show.legend = F) + geom_line(aes(x1,d11),data = data.frame(x1,d11)) + geom_line(aes(x2,d12),data=data.frame(x2,d12)) + xlab("") + ylab("") + ggtitle("Goal") +xlab("")
p2 = ggplot(s,aes(Minute)) + geom_density(aes(fill = I("red"), colour = I("red"),alpha=.5),show.legend = F) + geom_line(aes(x1,d21),data = data.frame(x1,d21)) + geom_line(aes(x2,d22),data=data.frame(x2,d22)) +ylab("") + xlab("")  + ggtitle("Save")
gridExtra::grid.arrange(p1, p2,ncol=2,bottom = "Minute",left = "Density")

#Shooter-Bernoulli
library(ggplot2)
ggplot(nhl)+geom_bar(aes(shooter,fill=shooter,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("")+xlab("Shooter position") +theme(legend.position="none")

#home
t = nhl$home
t[which(t==1)]="Home"
t[which(t==0)]="Away"
te = data.frame(t,"etype"=nhl$etype)
ggplot(te)+geom_bar(aes(t,fill=t,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("Goalie's team")+theme(legend.position="none")

#manpower
table(nhl$etype,nhl$manpower)
ggplot(nhl)+geom_bar(aes(manpower,fill=manpower,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("")+xlab("Opponent's manpower") +theme(legend.position="none")

#playoffs
t = nhl$playoffs
t[which(t==1)]="Playoff"
t[which(t==0)]="Regular"
te = data.frame(t,"etype"=nhl$etype)
ggplot(te)+geom_bar(aes(t,fill=t,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("Game type")+theme(legend.position="none")

#left shot
t = nhl$left_shot
t[which(t==1)]="Left"
t[which(t==0)]="Right"
te = data.frame(t,"etype"=nhl$etype)
ggplot(te)+geom_bar(aes(t,fill=t,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("Shot from goalie's perspective")+theme(legend.position="none")

#catch
ggplot(nhl)+geom_bar(aes(catch,fill=catch,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("Goalie catch hand") +theme(legend.position="none")


#Type-Multinomial
ggplot(nhl)+geom_bar(aes(type,fill=type,y = (..count..)/sum(..count..))) +facet_wrap(~etype)+ylab("Proportion")+xlab("Shot type")+scale_fill_discrete(name="Shot Type")+  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
                                                                                                                                                                                 
#Distance
library(MASS)
m_g_dist = fitdistr(g$distance, 'gamma')$estimate
s_g_dist = fitdistr(s$distance, 'gamma')$estimate
d1 = dgamma(g$distance, shape = m_g_dist[[1]], rate = m_g_dist[[2]])
d2 = dgamma(s$distance, shape = s_g_dist[[1]], rate = s_g_dist[[2]])
p1 = ggplot(g,aes(distance)) + geom_density(aes(fill=I("green"),colour = I("green"),alpha=.5),show.legend = F) + geom_line(aes(g,d1),data = data.frame("g"=g$distance,d1)) + xlab("") + ylab("") + ggtitle("Goal") +xlab("")
p2 = ggplot(s,aes(distance)) + geom_density(aes(fill = I("red"), colour = I("red"),alpha=.5),show.legend = F) + geom_line(aes(s,d2),data = data.frame("s"=s$distance,d2)) +ylab("") + xlab("")  + ggtitle("Save") + ylim(0,.05)
gridExtra::grid.arrange(p1, p2,ncol=2,bottom = "Shot distance (ft.)",left = "Density")


#angles
temp_g = g$angles + 90.021
temp_s = s$angles + 90.021
m_g_ang = fitdistr(temp_g, 'weibull')$estimate
s_g_ang = fitdistr(temp_s, 'weibull')$estimate
x1 = rweibull(1000,shape = m_g_ang[[1]], scale = m_g_ang[[2]])
x2 = rweibull(1000,shape = s_g_ang[[1]], scale = s_g_ang[[2]])
d1 = dweibull(x1, shape = m_g_ang[[1]], scale = m_g_ang[[2]])
d2 = dweibull(x2, shape = s_g_ang[[1]], scale = s_g_ang[[2]])
p1 = ggplot(g,aes(temp_g)) + geom_density(aes(fill=I("green"),colour = I("green"),alpha=.5),show.legend = F) + geom_line(aes(g,d1),data = data.frame("g"=x1,d1)) + xlab("") + ylab("") + ggtitle("Goal") +ylim(c(0,.03))
p2 = ggplot(s,aes(temp_s)) + geom_density(aes(fill = I("red"), colour = I("red"),alpha=.5),show.legend = F) + geom_line(aes(s,d2),data = data.frame("s"=x2,d2)) +ylab("") + xlab("")  + ggtitle("Save")+ylim(c(0,.03))
gridExtra::grid.arrange(p1, p2,ncol=2,bottom = "Shot angle (plus 90.021)",left = "Density")




#height
m_g_ht = fitdistr(g$height, 'normal')$estimate
s_g_ht = fitdistr(s$height, 'normal')$estimate
x1 = rnorm(1000,mean = m_g_ht[[1]], sd = m_g_ht[[2]])
x2 = rnorm(1000,mean = s_g_ht[[1]], sd = s_g_ht[[2]])
d1 = dnorm(x1, mean = m_g_ht[[1]], sd = m_g_ht[[2]])
d2 = dnorm(x2, mean = s_g_ht[[1]], sd = s_g_ht[[2]])
p1 = ggplot(g,aes(height)) + geom_density(aes(fill=I("green"),colour = I("green"),alpha=.5),show.legend = F) + geom_line(aes(g,d1),data = data.frame("g"=x1,d1)) + xlab("") + ylab("") + ggtitle("Goal") +ylim(c(0,1.4))
p2 = ggplot(s,aes(height)) + geom_density(aes(fill = I("red"), colour = I("red"),alpha=.5),show.legend = F) + geom_line(aes(s,d2),data = data.frame("s"=x2,d2)) +ylab("") + xlab("")  + ggtitle("Save") +ylim(c(0,1.4))
gridExtra::grid.arrange(p1, p2,ncol=2,bottom = "Goalie height (in.)",left = "Density")

#weight
m_g_wt = fitdistr(g$weight, 'normal')$estimate
s_g_wt = fitdistr(s$weight, 'normal')$estimate
x1 = rnorm(1000,mean = m_g_wt[[1]], sd = m_g_wt[[2]])
x2 = rnorm(1000,mean = s_g_wt[[1]], sd = s_g_wt[[2]])
d1 = dnorm(x1, mean = m_g_wt[[1]], sd = m_g_wt[[2]])
d2 = dnorm(x2, mean = s_g_wt[[1]], sd = s_g_wt[[2]])
p1 = ggplot(g,aes(weight)) + geom_density(aes(fill=I("green"),colour = I("green"),alpha=.5),show.legend = F) + geom_line(aes(g,d1),data = data.frame("g"=x1,d1)) + xlab("") + ylab("") + ggtitle("Goal") +ylim(c(0,.05))
p2 = ggplot(s,aes(weight)) + geom_density(aes(fill = I("red"), colour = I("red"),alpha=.5),show.legend = F) + geom_line(aes(s,d2),data = data.frame("s"=x2,d2)) +ylab("") + xlab("")  + ggtitle("Save") +ylim(c(0,.05))
gridExtra::grid.arrange(p1, p2,ncol=2,bottom = "Goalie weight (lbs.)",left = "Density")

#Replacing wrong angles
inds = which((nhl$angles < 13) & (nhl$angles > -10))
nhl$angles[inds] = runif(length(inds),-10,13)
temp = nhl$angles + 90.021
fitdistr(temp, "weibull")$estimate
plot(density(temp))
x = rweibull(100000,9.095694,158.438265)
a = which(x > -10+90.021 & x<13+90.021)
x1 = sample(x[a], 17873, replace = TRUE)
temp[inds] = x1
nhl$angles = temp - 90.021
plot(density(nhl$angles))
save(nhl, file="ProjectData3.Rda")



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
    return(dunif(x2,60,65)*overtime1)
  }
}
s_min = function(x) {
  if(x <=60) {
    return(dunif(x,1,60)*reg2)
  } else {
    return(dunif(x2,60,65)*overtime2)
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

X = nhl[,-1]
probs = array(NA,length(X[,1]))
for(i in 1:length(X[,1])) {
  probs[i] = par_prob(X[i,])
}
temp = data.frame(probs, "G" = nhl$etype)

#Parametric and empirical probabilities stored in "Predicted_Probabilities.Rdata"