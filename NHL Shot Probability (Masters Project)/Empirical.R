#Empirical probabilities
#nhl=read.csv("ProjectData.csv",header = T)
#Predicted probabilities for empirical model are in "PredictedProbabilities-Empirical.Rdata"
load("ProjectData3.Rda")
nhl = subset(nhl, Minute > 0 & Minute <=65)
#nhl = nhl[,-c(1,3)]
g = subset(nhl, etype == 'Goal')
s = subset(nhl,etype == 'Save')

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

X = nhl[,-1]
probs = array(NA,length(X[,1]))
for(i in 1:length(X[,1])) {
  probs[i] = e_prob(X[i,])
}
temp = data.frame(probs, "G" = nhl$etype)
empirical_probs = temp
save(empirical_probs, file= "PredictedProbabilities-Empirical.Rdata")
t1 = subset(temp,G==0)
t2 = subset(temp,G==1)
plot(density(t1$probs), xlab="Probability of a goal", main = "Naive Bayes Predicted Probs")
c1 = rgb(.9,0,0,alpha=.3)
c2 = rgb(0,.9,0,alpha=.3)
polygon(density(t1$probs),col=c1)
lines(density(t2$probs))
polygon(density(t2$probs),col=c2)
legend(.5,20,legend = c("Goal","Save"), col = c(c2,c1),pch=c(16,16))

#Joining data with goalies
load(file="final_coords.Rda",verbose = T)
load(file="Goalies2016.Rda",verbose=T)

m = data.frame("min"=unique(nhl$Minute))
shoot = data.frame("shooter"=unique(nhl$shooter))
type = data.frame("shot_type"=unique(nhl$type))
home = data.frame("home"=unique(nhl$home))
man = data.frame("man"=unique(nhl$manpower))
play = data.frame("playoff"=unique(nhl$playoffs))
#Don't run
#mass_data = sqldf("select * from goalie,final_coords,m,shoot,type,home,man,play")



#Obtaining baseline probabilities from empirical densities
baseline_probs = function(angle,distance,left) {
  top = as.numeric(g_prior)*as.numeric(g_dist(distance))*as.numeric(g_ang(angle))*as.numeric(g_left[1+left]);
  bot = as.numeric(s_prior)*as.numeric(s_dist(distance))*as.numeric(s_ang(angle))*as.numeric(s_left[1+left])
  return(data.frame(top,bot))
}
base_probs = data.frame()
for(i in 1:length(final_coords[,1])) {
  base_probs = rbind(base_probs,baseline_probs(final_coords$Angle[i], final_coords$Distance[i],final_coords$Left[i]))
}
final_coords = data.frame(final_coords, base_probs)

#Using ggplot2
library(jpeg)
img <- readJPEG("t.jpg")
g=as.raster(img,alpha=.01)
library(ggplot2)
ggplot(final_coords, aes(x, y))+annotation_raster(g, xmin=-100,xmax=100,ymin=-42,ymax=42.5)+ geom_tile(aes(fill = top/(top+bot), alpha=1)) +scale_fill_gradient(low = "red", high = "green")+scale_alpha_continuous(range=c(0.7,.9))
ggplot(nhl, aes(distance, fill = factor(etype), alpha = I(.1))) + geom_density() 

ggplot(final_coords,aes(x,y)) + geom_tile()


#Removing coordinates that the nets cover
inds = which(((final_coords$x %in% -93:-90)&(final_coords$y %in% -3:3)) |  ((final_coords$x %in% 90:93)&(final_coords$y %in% -3:3))) 
final_coords = final_coords[-inds,]
#Removing coordinates that go out of the rink (corners)
temp = final_coords
for(i in 2:11) {
  inds = which((abs(temp$x) > (88+i)) & abs(temp$y) == 44-i)
  temp = temp[-inds,]
}
inds = which((abs(temp$x) == 100) | (abs(temp$y) == 42))
temp=temp[-inds,]
ggplot(temp, aes(x, y))+ geom_tile(aes(fill = top/(top+bot))) +scale_fill_gradient(low = "blue", high = "green")+annotation_raster(g, xmin=-100,xmax=100,ymin=-42,ymax=42)
final_coords = temp


#_______Function to dynamically calculate probabilities
empirical_probs = function(minute = NA, shooter = NA, type = NA, home = NA, manpower = NA, playoffs = NA, height = NA, weight = NA, catch = NA) {
  t = final_coords$top*ifelse(is.na(minute),1, g_min(minute))*ifelse(is.na(shooter),1, g_shoot[shooter])*ifelse(is.na(type),1, g_type[type])*ifelse(is.na(home),1, g_home[1+home])*ifelse(is.na(manpower),1, g_man[manpower])*ifelse(is.na(playoffs),1, g_play[1+playoffs])*ifelse(is.na(height),1, g_height(height))*ifelse(is.na(weight),1, g_weight(weight))*ifelse(is.na(catch),1, g_catch[catch]);
  b = final_coords$bot*ifelse(is.na(minute),1, s_min(minute))*ifelse(is.na(shooter),1, s_shoot[shooter])*ifelse(is.na(type),1, s_type[type])*ifelse(is.na(home),1, s_home[1+home])*ifelse(is.na(manpower),1, s_man[manpower])*ifelse(is.na(playoffs),1, s_play[1+playoffs])*ifelse(is.na(height),1, s_height(height))*ifelse(is.na(weight),1, s_weight(weight))*ifelse(is.na(catch),1, s_catch[catch]);
  return(t/(t+b))
  }

#In paper
  #Densities of continuous variables in each class
  #Plot densities of predicted probabilities with the truth
  #ROC Curves
+scale_fill_gradient(low = "red", high = "green")



#_____Using 10 fold cross validation for predicted probabilities__________________
load("ProjectData3.Rda")
nhl = subset(nhl, Minute > 0 & Minute <=65)
#nhl = nhl[,-c(1,3)]
g = subset(nhl, etype == 'Goal')
s = subset(nhl,etype == 'Save')

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

X = nhl[,-1]
probs = array(NA,length(X[,1]))
for(i in 1:length(X[,1])) {
  probs[i] = e_prob(X[i,])
}