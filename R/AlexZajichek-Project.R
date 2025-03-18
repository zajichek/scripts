#Applied Time Series Project
#Comparison of the Home Price Index of WI to Midwest to USA
#Spring 2017, Osnat Stramer
#Author: Alex Zajichek

setwd("~/Documents/School/Iowa-M.S. Statistics/Spring 2017/Applied Time Series/Project")

#Plotting all 3 series together
load(file = "natl_HPI.Rda", verbose = T)
load(file = "midwst_HPI.Rda", verbose = T)
load(file = "WI_HPI.Rda", verbose = T)
all_series <- data.frame("Type" = c(rep("National", 102), rep("Midwest", 102), rep("Wisconsin", 102)), "Year" = rep(natl_HPI$Year, 3), "HPI" = c(natl_HPI$Index_NSA, midwst_HPI$HPI, WI_HPI$index_nsa)) 
library(ggplot2)
ggplot(all_series) + geom_line(aes(x = Year, y = HPI, colour = Type))

library(TSA)
#Step 1: Variance Stabilizing Tranformations
par(mfrow = c(1,3))
BC1 <- BoxCox.ar(natl_HPI$Index_NSA, method = 'ols', lambda = seq(-3,3,.01))
BC2 <- BoxCox.ar(midwst_HPI$HPI, method = 'ols', lambda = seq(-3,3,.01))
BC3 <- BoxCox.ar(WI_HPI$index_nsa, method = 'ols', lambda = seq(-3,3,.01))
BC1$mle #-0.9
BC1$ci #-1.45 -0.34 <-- use -1.0
BC2$mle #-1.79
BC2$ci #-2.36 -1.20 <-- use -1.5
BC3$mle #-0.56
BC3$ci #-1.14 0.03 <-- use -0.5

#Step 2: Obtain stationary series
national <- (natl_HPI$Index_NSA^-1 - 1)/(-1)
midwest <- (midwst_HPI$HPI^(-1.5) - 1)/(-1.5)
wisc <- (WI_HPI$index_nsa^(-0.5) - 1)/(-0.5)
transed <- data.frame("Type" = c(rep("National", 102), rep("Midwest", 102), rep("Wisconsin", 102)), "Year" = rep(natl_HPI$Year, 3), "HPI" = c(national, midwest, wisc)) 
ggplot(transed) + geom_line(aes(x = Year, y = HPI, colour = Type)) + facet_wrap(~Type, scales = "free", nrow=3) + ylab("Transformed HPI") + theme(legend.position="none")

    #Linear trend does not seem to hold, difference will be taken
adf.test(national) #pvalue for Y_t: 0.5237
adf.test(midwest) #pvalue for Y_t: 0.4488
adf.test(wisc) #pvalue for Y_t: 0.5581

library(TSA)
#First difference
nat_diff <- diff(national); 
adf.test(nat_diff) #0.6559
mid_diff <- diff(midwest); 
adf.test(mid_diff) #.7507
WI_diff <- diff(wisc); 
adf.test(WI_diff) #0.6327
#Second difference
nat_diff2 <- diff(nat_diff); 
adf.test(nat_diff2) #<0.01
mid_diff2 <- diff(mid_diff); 
adf.test(mid_diff2) #<0.01
WI_diff2 <- diff(WI_diff); 
adf.test(WI_diff2) #<0.01

library(ggplot2)
transed <- data.frame("Type" = c(rep("National", 101), rep("Midwest", 101), rep("Wisconsin", 101)), "Year" = rep(natl_HPI$Year[-1], 3), "HPI" = c(nat_diff, mid_diff, WI_diff)) 
p1 <- ggplot(transed) + geom_line(aes(x = Year, y = HPI, colour = Type)) + facet_wrap(~Type, scales = "free", ncol=3) + ylab("First Difference") + theme(legend.position="none")
transed2 <- data.frame("Type" = c(rep("National", 100), rep("Midwest", 100), rep("Wisconsin", 100)), "Year" = rep(natl_HPI$Year[-c(1,2)], 3), "HPI" = c(nat_diff2, mid_diff2, WI_diff2)) 
p2 <- ggplot(transed2) + geom_line(aes(x = Year, y = HPI, colour = Type)) + facet_wrap(~Type, scales = "free", ncol=3) + ylab("Second Difference") + theme(legend.position="none")
gridExtra::grid.arrange(p1,p2)

#Step 3: Find pool of models for second diff
par(mfrow=c(1,3))
acf(mid_diff2, col = "red", main = "Midwest", xlab = "") #AR(2) MA(4)
acf(nat_diff2, col = "darkgreen", main = "National", ylab = "") #AR(2) MA(3)
acf(WI_diff2, col = "blue", main = "Wisconsin", ylab = "", xlab = "") #AR(2) MA(2) MA(4)
par(mfrow=c(1,3))
pacf(mid_diff2, col = "red", main = "Midwest", xlab = "") #AR(3) #AR(5)
pacf(nat_diff2, col = "darkgreen", main = "National", ylab = "") #AR(2)
pacf(WI_diff2, col = "blue", main = "Wisconsin", ylab = "", xlab = "") #AR(3)
eacf(nat_diff2) #ARMA(2,3)
AR/MA
0 1 2 3 4 5 6 7 8 9 10 11 12 13
0 x x x o o o o o x o x  o  o  x 
1 x x x o o o o o x o o  o  o  x 
2 o o o o o o o o o o o  o  o  o 
3 x o o o o o o o o o o  o  o  o 
4 x o x o o x o o o o o  o  o  o 
5 x o x o o x o o o o o  o  o  o 
6 o o x o o x o o o o o  o  o  o 
7 o x x o o x o o o o o  o  o  o 
eacf(mid_diff2) #ARMA(2,6)
AR/MA
0 1 2 3 4 5 6 7 8 9 10 11 12 13
0 o x o x o o o o x o o  o  o  o 
1 x x o x x o o o x o x  o  x  o 
2 x o x x o x o o o o o  o  o  o 
3 o o x x o x o o o o o  o  o  o 
4 o x x x o x o o o o o  o  o  o 
5 x x x o o x o o o o o  o  o  o 
6 x o x o o x o o o o o  o  o  o 
7 x o x x o o o o o o o  o  o  o 
eacf(WI_diff2)  #MA(4)
AR/MA
0 1 2 3 4 5 6 7 8 9 10 11 12 13
0 x x o x o o o o o o o  o  o  o 
1 x x o x o o o o o o o  o  o  o 
2 x x x x o o o o o o o  o  o  o 
3 o x o x o o o o o o o  o  o  o 
4 x x o x o o o o o o o  o  o  o 
5 o x o o o o o o o o o  o  o  o 
6 o x o o o o o o o o o  o  o  o 
7 x x o o o o o o o o o  o  o  o 
par(mfrow = c(1,3))
plot(armasubsets(mid_diff2, nar = 12, nma = 12), col = "red"); title("Midwest")
plot(armasubsets(nat_diff2, nar = 12, nma = 12), col = "darkgreen"); title("National")
plot(armasubsets(WI_diff2, nar = 12, nma = 12), col = "blue"); title("Wisconsin")
#ARMA(9, 1) with zeros
#AR(2) AR(3)

#Step 3: Fit models and determine AICc
AICc <- function(model, params, n) {
    -2*logLik(model) + (2*(params + 1)*n)/(n - params - 2)
}
#AICc       #National   #Midwest    #Wisconsin
#AR(2)      -1826.012*   -2362.055   -1221.921            
#AR(3)      -1824.055   -2370.101   -1232.339*
#AR(5)      -1821.335   -2373.637*   -1230.323
#MA(3)      -1820.946   -2363.714   -1228.139
#MA(4)      -1822.218   -2372.163   -1228.611
#ARMA(2,3)  -1826.98*    -2367.418   -1228.54
#ARMA(2,6)  -1824.153   -2368.114   -1227.224
#ARMA(9,1)  -1824.934   -2373.077*   -1232.435*

#Best two models for each can be delved further


#Step 4: Choose final model for each based prediction on power transformed data
library(TSA)
prediction_error <- function(model, newdata) {
    preds <- predict(model, length(newdata))$pred
    mean(abs(newdata - preds))*10000
}
#National ARIMA(2,0,0) ARIMA(2,0,3)
inds <- 1:95
prediction_error(arima(nat_diff2[inds], order = c(2,0,0),include.mean = F), nat_diff2[-inds])
prediction_error(arima(nat_diff2[inds], order = c(2,0,3),include.mean = F), nat_diff2[-inds])
# 0.125, 0.142

#Midwest ARIMA(5,0,0) ARIMA(9,0,1) w/ zeros
prediction_error(arima(mid_diff2[inds], order = c(5,0,0), include.mean = F), mid_diff2[-inds])
prediction_error(arima(mid_diff2[inds], order = c(9,0,1), include.mean = F, fixed = c(0,NA,0,0,0,0,0,0,NA,NA)), mid_diff2[-inds])
#0.00994, 0.00987

#Wisconsin AR(3) ARMA(9,1) w/ zeros
prediction_error(arima(WI_diff2[inds], order = c(3,0,0), include.mean = F), WI_diff2[-inds])
prediction_error(arima(WI_diff2, order = c(9,0,1), include.mean = F, fixed = c(0,NA,0,0,0,0,0,0,NA,NA)), WI_diff2[-inds])
#2.072, 4.0802


#Final models
mod_nat <- arima(nat_diff2, order = c(2,0,0), include.mean = F)
mod_mid <- arima(mid_diff2, order = c(9,0,1), include.mean = F, fixed = c(0,NA,0,0,0,0,0,0,NA,NA))
mod_wi <- arima(WI_diff2, order = c(3,0,0), include.mean = F)

#Step 5: Residual diagnostics
library(ggplot2)
library(TSA)
res_nat <- rstandard(mod_nat)
res_mid <- rstandard(mod_mid)
res_wi <- rstandard(mod_wi)
res <- data.frame("Type" = c(rep("National", 100), rep("Midwest", 100), rep("Wisconsin", 100)), "Residual" = c(res_nat, res_mid, res_wi))
ggplot(res) + stat_qq(aes(sample = Residual, colour = Type)) + geom_abline(intercept = 0, slope = 1, linetype = 2) + facet_wrap(~Type, nrow =3)+ theme(legend.position="none") + xlab("Theoretical Quantiles") + ylab("Sample Quantile")
ggplot(res)  + geom_density(aes(Residual, fill = Type), adjust = 1/4) + facet_wrap(~Type) + ylab("Density") + xlab("Standardized Residual")+ theme(legend.position="none")
shapiro.test(res_nat) #.598
shapiro.test(res_mid) #.5846
shapiro.test(res_wi) #.01995
runs(res_nat)$pvalue #0.918
runs(res_mid)$pvalue #0.84
runs(res_wi)$pvalue #0.652
tsdiag(mod_nat)
tsdiag(mod_mid)
tsdiag(mod_wi)
par(mfrow=c(1,3))
pacf(res_mid, main = "Midwest", col = "red")
pacf(res_nat, main = 'National', col = "darkgreen")
pacf(res_wi, main = "Wisconsin", col = "blue")
par(mfrow=c(1,3))
acf(res_mid, main = "Midwest", col = "red")
acf(res_nat, main = 'National', col = "darkgreen")
acf(res_wi, main = "Wisconsin", col = "blue")

#Step 6: Fit models and forecast new HPI
nat <- arima(national, order = c(2,2,0), include.mean = F)
nat_preds <- predict(nat, 12)
nat_fit <- nat_preds$pred
nat_lower <- nat_fit - 2*nat_preds$se
nat_upper <- nat_fit + 2*nat_preds$se

mid <- arima(midwest, order = c(9,2,1), include.mean = F, fixed = c(0,NA,0,0,0,0,0,0,NA,NA))
mid_preds <- predict(mid, 12)
mid_fit <- mid_preds$pred
mid_lower <- mid_fit - 2*mid_preds$se
mid_upper <- mid_fit + 2*mid_preds$se

wi <- arima(wisc, order = c(3,2,0), include.mean = F)
wi_preds <- predict(wi, 12)
wi_fit <- wi_preds$pred
wi_lower <- wi_fit - 2*wi_preds$se
wi_upper <- wi_fit + 2*wi_preds$se

inv_trans <- function(y, lambda) {
    (y*lambda + 1)^(1/lambda)
}
#Transforming back to orginial units
nat_fit <- inv_trans(nat_fit, -1)
nat_lower <- inv_trans(nat_lower, -1)
nat_upper <- inv_trans(nat_upper, -1)

mid_fit <- inv_trans(mid_fit, -1.5)
mid_lower <- inv_trans(mid_lower, -1.5)
mid_upper <- inv_trans(mid_upper, -1.5)

wi_fit <- inv_trans(wi_fit, -.5)
wi_lower <- inv_trans(wi_lower, -.5)
wi_upper <- inv_trans(wi_upper, -.5)

prediction_intervals <- data.frame("Year" = rep(c(natl_HPI$Year, seq(2016.50, 2019.25,.25)),3)
           ,"Method" = c(rep("Midwest", 114), rep("National", 114), rep("Wisconsin", 114))
           , "Lower" = c(rep(0,102),mid_lower,rep(0,102),nat_lower,rep(0,102),wi_lower)
           , "HPI" = c(midwst_HPI$HPI, mid_fit,natl_HPI$Index_NSA,nat_fit,WI_HPI$index_nsa,wi_fit)
           , "Upper" = c(rep(0,102),mid_upper,rep(0,102),nat_upper,rep(0,102),wi_upper)
           )
pi <- prediction_intervals
pi$Lower[pi$Lower == 0] <- pi$HPI[pi$Lower == 0]
pi$Upper[pi$Upper == 0] <- pi$HPI[pi$Upper == 0]
prediction_intervals <- pi
ggplot(prediction_intervals) + geom_line(aes(x = Year, y = HPI, colour = Method, linetype = Method)) +
   geom_ribbon(aes(x = Year, ymin = Lower, ymax = Upper, colour = Method, fill = Method), alpha=.3, colour = NA) +
ylab("Home Price Index")

ggplot(prediction_intervals[prediction_intervals$Year >= 2016,]) + geom_line(aes(x = Year, y = HPI, colour = Method, linetype = Method)) +
    geom_ribbon(aes(x = Year, ymin = Lower, ymax = Upper, colour = Method, fill = Method), alpha=.3, colour = NA) +
    ylab("Home Price Index")









