#all predicted probabilities from 10 fold CV in 'pred_probs.Rdata'
load("pred_probs.Rdata",verbose = T)
truth = as.numeric(predicted_probs$Truth=='Goal')
#Test errors
te = function(x,p) {
  temp = as.numeric(x>=p)
  return((length(which((temp == 1 & truth == 0)|(temp==0 & truth==1))))/length(truth))
}
p=seq(0,1,by=.01)
e_te = matrix(NA,ncol=1,nrow=length(p))
p_te = matrix(NA,ncol=1,nrow=length(p))
l_te = matrix(NA,ncol=1,nrow=length(p))
for(i in 1:length(p)) {
  e_te[i,1] = te(predicted_probs$Empirical,p[i]) 
  p_te[i,1] = te(predicted_probs$Parametric,p[i]) 
  l_te[i,1] = te(predicted_probs$Logistic,p[i]) 
}
e_r = data.frame("Error_Rate"=e_te[,1])
p_r = data.frame("Error_Rate"=p_te[,1])
l_r = data.frame("Error_Rate"=l_te[,1])
errors = data.frame("P"=p,"Model"=c(rep("Empirical",length(e_r[,1])),rep("Parametric",length(p_r[,1])),rep("Logistic",length(e_r[,1]))), rbind(e_r,p_r,l_r))

#Only looking at true saves
fp=function(x,p) {
  temp = as.numeric(x>=p)
  length(which(truth == 0 & temp == 1))/length(which(truth == 0))
}
p=seq(0,1,by=.01)
e_fp = matrix(NA,ncol=1,nrow=length(p))
p_fp = matrix(NA,ncol=1,nrow=length(p))
l_fp = matrix(NA,ncol=1,nrow=length(p))
for(i in 1:length(p)) {
  e_fp[i,1] = fp(predicted_probs$Empirical,p[i]) 
  p_fp[i,1] = fp(predicted_probs$Parametric,p[i]) 
  l_fp[i,1] = fp(predicted_probs$Logistic,p[i]) 
}
e_fp = data.frame("Error_Rate"=e_fp[,1])
p_fp= data.frame("Error_Rate"=p_fp[,1])
l_fp = data.frame("Error_Rate"=l_fp[,1])
fpos = data.frame("P"=p,"Model"=c(rep("Empirical",length(e_fp[,1])),rep("Parametric",length(p_fp[,1])),rep("Logistic",length(e_fp[,1]))), rbind(e_fp,p_fp,l_fp))

#Only looking at true goals
fn=function(x,p) {
  temp = as.numeric(x>=p)
  length(which(truth == 1 & temp == 0))/length(which(truth == 1))
}
p=seq(0,1,by=.01)
e_fn = matrix(NA,ncol=1,nrow=length(p))
p_fn = matrix(NA,ncol=1,nrow=length(p))
l_fn = matrix(NA,ncol=1,nrow=length(p))
for(i in 1:length(p)) {
  e_fn[i,1] = fn(predicted_probs$Empirical,p[i]) 
  p_fn[i,1] = fn(predicted_probs$Parametric,p[i]) 
  l_fn[i,1] = fn(predicted_probs$Logistic,p[i]) 
}
e_fn = data.frame("Error_Rate"=e_fn[,1])
p_fn = data.frame("Error_Rate"=p_fn[,1])
l_fn = data.frame("Error_Rate"=l_fn[,1])
fneg = data.frame("P"=p,"Model"=c(rep("Empirical",length(e_fn[,1])),rep("Parametric",length(p_fn[,1])),rep("Logistic",length(e_fn[,1]))), rbind(e_fn,p_fn,l_fn))

error = data.frame(errors, "FN_Rate"=fneg$Error_Rate, "FP_Rate"=fpos$Error_Rate, "TP_Rate"=1-fneg$Error_Rate)
save(error,file="Test_errors-CV.Rdata")


#___Plotting various test errors
load("Test_errors-CV.Rdata",verbose = T)
library(ggplot2)
#ROC Curve
p1=ggplot(error)+geom_line(aes(FP_Rate,TP_Rate,colour=Model))+geom_line(data=data.frame("X"=c(0,1),"Y"=c(0,1)),aes(X,Y))+ylab("True positive rate")+xlab("False positive rate")
#Error rate curve
p2=ggplot(error)+geom_line(aes(P,Error_Rate,colour=Model))+ylab("Error rate")+xlab("Cutoff")+ylim(c(0,1))
#False positives (classifying as goal when it was a save)
p3=ggplot(error)+geom_line(aes(P,FP_Rate,colour=Model))+ylab("False positive rate")+xlab("Cutoff")
#False negatives (classifying as save when it was a goal)
p4=ggplot(error)+geom_line(aes(P,FN_Rate,colour=Model))+ylab("False negative rate")+xlab("Cutoff")
gridExtra::grid.arrange(p1,p2,p3,p4)

library(MESS)
e_res = subset(error,Model=="Empirical")
p_res = subset(error,Model=="Parametric")
l_res = subset(error,Model=="Logistic")
MESS::auc(e_res$FP_Rate,e_res$TP_Rate,0,1)
[1] 0.7117464
MESS::auc(p_res$FP_Rate,p_res$TP_Rate,0,1)
[1] 0.7122127
MESS::auc(l_res$FP_Rate,l_res$TP_Rate,0,1)
[1] 0.7351413

#Finding optimal cutoffs___________
e = subset(error,Model=="Empirical")
p = subset(error,Model=="Parametric")
l = subset(error,Model=="Logistic")
#Empirical
e_1=approxfun(e$P,e$FN_Rate)
e_2=approxfun(e$P,e$FP_Rate)
e_3=approxfun(e$P,e$Error_Rate)
temp=seq(0,.2,by=.00001)
res = array(NA,length(temp))
for(i in 1:length(temp)){
  res[i]=abs(e_1(temp[i])-e_2(temp[i]))
}
temp[which.min(res)]
[1] 0.05664
e_1(.05664)
[1] 0.3339232
e_2(.05664)
[1] 0.3339255

#Parametric
p_1=approxfun(p$P,p$FN_Rate)
p_2=approxfun(p$P,p$FP_Rate)
p_3=approxfun(p$P,p$Error_Rate)
temp=seq(0,.5,by=.00001)
res = array(NA,length(temp))
for(i in 1:length(temp)){
  res[i]=abs(p_1(temp[i])-p_2(temp[i]))
}
temp[which.min(res)]
[1] 0.09123
p_1(.09123)
[1] 0.3325504
p_2(.09123)
[1] 0.332541

#Parametric
l_1=approxfun(l$P,l$FN_Rate)
l_2=approxfun(l$P,l$FP_Rate)
l_3=approxfun(l$P,l$Error_Rate)
temp=seq(0,.5,by=.00001)
res = array(NA,length(temp))
for(i in 1:length(temp)){
  res[i]=abs(l_1(temp[i])-l_2(temp[i]))
}
temp[which.min(res)]
[1] 0.0932
l_1(.0932)
[1] 0.316947
l_2(.0932)
[1] 0.3169055

#1=False negative
#2=False positive
#3=Test error
#order of values (empirical cut, parametric cut, logistic cut)
cutoffs=c(.05664,.09123,.0932)
e_1(cutoffs)
0.3339232 0.5212627 0.5314545
e_2(cutoffs)
0.3339255 0.1995361 0.1939510
e_3(cutoffs)
0.3339253 0.2271626 0.2229323
p_1(cutoffs)
0.1971165 0.3325504 0.3396051
p_2(cutoffs)
0.4974273 0.3325410 0.3258520
p_3(cutoffs)
0.4716397 0.3325418 0.3270330
l_1(cutoffs)
0.1505129 0.3078484 0.3169470
l_2(cutoffs)
0.5262382 0.3246627 0.3169055
l_3(cutoffs)
0.4939749 0.3232189 0.3169090
