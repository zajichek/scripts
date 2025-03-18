#Connecting to the nhl.sqlite database
library(RSQLite)
con = dbConnect(drv = SQLite(), dbname = "nhl.sqlite")

#Selecting initial dataset from database
query = "SELECT etype, playbyplay.season, playbyplay.gcode AS gcode, refdate, seconds, [ev.team] AS shoot_team, pos AS shooter, 
type, homezone, distance, xcoord, ycoord, playbyplay.awayteam, playbyplay.hometeam, [away.G] AS away_goalie, [home.G] AS home_goalie, [home.skaters] AS hm_skaters, 
[away.skaters] AS aw_skaters
FROM playbyplay, players WHERE (playbyplay.season = players.Season 
AND playbyplay.gcode = players.gcode AND playbyplay.[ev.player.1] = players.numfirstlast)
AND playbyplay.season >= '20072008' AND 
(etype = 'GOAL' OR etype = 'SHOT') AND type != 'Unspecified' AND 
xcoord IS NOT NULL and ycoord IS NOT NULL AND distance IS NOT NULL AND seconds != 3900"

nhl = dbGetQuery(con,query)

#Converting each goal to numeric indicator
nhl$etype = as.numeric(nhl$etype == 'GOAL')

#Finding goalie who shot was taken on
goalie = array(NA, length(nhl[,1]))
home = array(NA,length(nhl[,1]))
for(i in 1:length(goalie)) {
  if(nhl$shoot_team[i] == nhl$awayteam[i]) {
    goalie[i] = nhl$home_goalie[i];
    home[i] = 1
  } else if(nhl$shoot_team[i] == nhl$hometeam[i]) {
    goalie[i] = nhl$away_goalie[i];
    home[i] = 0
  }
}
nhl = cbind(nhl,goalie,home)
#Removing empty net goals scored
nhl = nhl[-which(goalie == ""), ]
#Removing home/away goalies
nhl = nhl[,-c(15,16)]
#Converting seconds to minutes
nhl$seconds = ceiling(nhl$seconds/60)
#Converting shooter position to forward or defense (omitting cases where the goalie shot)
nhl = nhl[-which(nhl$shooter == 'G'),]
nhl$shooter[which(nhl$shooter %in% c('C','L','R'))] = 'F'
#Removing cases where hm_skaters or aw_skaters < 4
nhl = nhl[-which(nhl$hm_skaters %in% c(1,2) | nhl$aw_skaters %in% c(1,2)),] #12 cases

#Making manpower variable
manpower = array("", length(nhl[,1]))
on_ice = paste(nhl$hm_skaters,nhl$aw_skaters)
nhl = cbind(nhl, manpower, on_ice)
  #Even strength
nhl$manpower[which(on_ice %in% c('6 6','5 5','4 4'))] = 'Even'  #Run this line first before "pulled"
  #Pulled goalie
nhl$manpower[which(nhl$away_goalie == "" | nhl$home_goalie == "")] = "Pulled"
  #Short handed #FROM THE GOALIES PERSPECTIVE (HIS TEAM IS SHORT HANDED)
nhl$manpower[which(nhl$home == 1 & nhl$manpower != "Pulled" & (nhl$on_ice == '5 6' | nhl$on_ice == '4 5' | nhl$on_ice == '4 6'))] = "Short"
nhl$manpower[which(nhl$home == 0 & nhl$manpower != "Pulled" & (nhl$on_ice == '6 5' | nhl$on_ice == '5 4' | nhl$on_ice == '6 4'))] = "Short"  
  #PowerPlay #FROM THE GOALIES PERSPECTIVE (HIS TEAM IS ON A POWERPLAY)
nhl$manpower[which(nhl$home == 0 & nhl$manpower != "Pulled" & (nhl$on_ice == '5 6' | nhl$on_ice == '4 5' | nhl$on_ice == '4 6'))] = "Power"
nhl$manpower[which(nhl$home == 1 & nhl$manpower != "Pulled" & (nhl$on_ice == '6 5' | nhl$on_ice == '5 4' | nhl$on_ice == '6 4'))] = "Power" 
  
#Removing redundant information
nhl = nhl[,-c(15,16,17,18,22)]
nhl = nhl[,-c(2,6,13,14)]
names(nhl)[4] = "Minute"
#playoffs
playoffs = sqldf("SELECT CASE WHEN gcode LIKE '3%' THEN 1 ELSE 0 END AS playoffs FROM nhl[,2]")
nhl = cbind(nhl, "playoffs" = playoffs[,1])


#Getting angles of shots
nhl = load("ProjectDataWithCoordinates.Rda",verbose =TRUE)
nhl = temp2
rm(temp2)
names(nhl)[c(14,15)] = c("xcoord","ycoord")
inds = which(nhl$distance < 100 & abs(nhl$xcoord) > 89)
temp = nhl[-inds,]
angles = acos(abs(temp$ycoord)/temp$distance)*(180/pi)
na_angles = which(is.na(angles))
temp2 = temp[na_angles,]
a = atan((89-abs(temp2$xcoord))/abs(temp2$ycoord))*(180/pi)
temp2$distance = sqrt((89-abs(temp2$xcoord))^2 + abs(temp2$ycoord)^2)
temp2$distance = round(temp2$distance)
angles[na_angles] = a
temp = cbind(temp[-na_angles,], "angles"=angles[-na_angles])
temp2 = cbind(temp2, "angles" = a)
temp = rbind(temp, temp2)
temp2 = nhl[inds,]
angles = atan((abs(temp2$xcoord) - 89)/abs(temp2$ycoord))*(180/pi)
angles = angles*-1
temp2$distance = sqrt((abs(temp2$xcoord) - 89)^2 + abs(temp2$ycoord)^2)
temp2 = cbind(temp2, "angles" = angles)
nhl = rbind(temp,temp2)


#Variable Descriptions
#etype-'1' indicates goal scored, '0' indicates save
#refdate - number of days since '2002-01-01'
#Minute - In the i_th minute of the game
#shooter - Position of shooter ('F' or 'D')
#type - shot type by shooter
#distance - distance shot was taken from in feet
#goalie - goaltender in net that shot was taken on
#home - '1' indicates goalie is on the home team, '0' is away
#manpower - 'Even', 'Pulled', 'Short', 'Powerplay' 
  #Shooting team has 'Pulled' goalie, goalie's team is 'Short' or on 'Powerplay'
#playoffs - '1' if playoff game, '0' if not
#angles - angle shot was taken from
#left_shot-'1' if shot was taken from left side of ice from shooter's perspective, '0' if from the right 
  #center of the net is the dividing line

temp = strsplit(nhl$goalie, " ")
f = function(x) {
  paste(x[3], x[2])
}
temp2 = lapply(temp, f)
temp = unlist(temp2)
nhl = cbind(nhl, temp)
temp = merge(nhl,g, by = "Name", all.x = TRUE)
temp = arrange(temp,X)
#Allgoalies.csv contains all 580381 rows but some data is still missing
g$Name[which(g$Name == 'BRYZGALOV ILJA')] = 'BRYZGALOV ILYA'
g$Name[which(g$Name == 'AULD ALEX')] = 'AULD ALEXANDER'
g$Name[which(g$Name == 'DROUIN-DESLAURIERS JEFF')] = 'DESLAURIERS JEFF'
g$Name[which(g$Name == 'FERNANDEZ EMMANUEL')] = 'FERNANDEZ MANNY'
g$Name[which(g$Name == 'HOWARD JAMES')] = 'HOWARD JIMMY'
g$Name[which(g$Name == 'JR. TIMOTHY')] = 'THOMAS TIM'
g$Name[which(g$Name == 'LEGACE EMMANUEL')] = 'LEGACE MANNY'
g$Name[which(g$Name == 'MCCOLLUM THOMAS')] = 'MCCOLLUM TOM'
g$Name[which(g$Name == 'VALIQUETTE STEPHEN')] = 'VALIQUETTE STEVE'
g$Name[which(g$Name == 'VARLAMOV SIMEON')] = 'VARLAMOV SEMYON'

#___Estimating conditional parameters for Naive Bayes
nhl = read.csv("ProjectData.csv",header=TRUE)
nhl = nhl[,-c(1,3)] #Remove index and date
nhl = subset(nhl, Minute > 0 & Minute <= 65) 
g = subset(nhl, etype == 1)
s = subset(nhl, etype == 0)
rm(nhl)
library(MASS)
#distance
fitdistr(g$distance, "gamma")$estimate
shape      rate 
2.4013322 0.1011266 
fitdistr(s$distance, "gamma")$estimate
shape       rate 
2.86207435 0.07821654 
#angle
Ag = -1*g$angles+90.00001
As = -1*s$angles + 90.00001
fitdistr(Ag, "gamma")$estimate
shape      rate 
0.5306953 0.0207826
fitdistr(As, "gamma")$estimate
shape       rate 
0.89525506 0.02932623
#Plotting empirical densities v.s. MLE estimates
#Minute
par(mfrow=c(2,2))
x1 = sort(runif(length(which(g$Minute<=60)),1,60))
x2 = sort(runif(length(which(g$Minute>60)), 60,65))
x3 = sort(runif(length(which(s$Minute<=60)),1,60))
x4 = sort(runif(length(which(s$Minute>60)), 60,65))
plot(density(g$Minute[which(g$Minute<=60)]), main = "Goals",xlab="Minute",ylab="Density")
lines(x1,dunif(x1,1,60),col="red")
plot(density(g$Minute[which(g$Minute>60)]), main = "Overtime Goals",xlab="Minute",ylab="Density")
lines(x2,dunif(x2,60,65),col="red")
plot(density(s$Minute[which(s$Minute<=60)]), main = "Shots",xlab="Minute",ylab="Density")
lines(x3,dunif(x3,1,60),col="red")
plot(density(s$Minute[which(s$Minute>60)]), main = "Overtime Shots",xlab="Minute",ylab="Density")
lines(x4,dunif(x4,60,65),col="red")
#Distance
par(mfrow=c(1,2))
x1=sort(rgamma(length(g$distance), 2.4013322,0.1011266))
x2=sort(rgamma(length(s$distance),2.86207435,0.07821654))
plot(density(g$distance),main="Goals",xlab="Distance",ylab="Density")
lines(x1,dgamma(x1,2.4013322,0.1011266),col="red")
plot(density(s$distance),main="Shots",xlab="Distance",ylab="Density")
lines(x2,dgamma(x2,2.86207435,0.078211654),col="red")
#Angle
par(mfrow=c(1,2))
x1=sort(rgamma(length(g$angles),.5306953,.0207826))
x2=sort(rgamma(length(s$angles),.89525506,.02932623))
plot(density(Ag),main="Goals",xlab="-1*Angle+90",ylab="Density")
lines(x1,dgamma(x1,.5306953,.0207826),col="red")
plot(density(As),main="Goals",xlab="-1*Angle+90",ylab="Density")
lines(x2,dgamma(x2,.89525506,.02932623),col="red")
#Height
par(mfrow=c(1,2))
x1 = sort(rnorm(length(g$height),mean(g$height),sd(g$height)))
x2 = sort(rnorm(length(s$height),mean(s$height),sd(s$height)))
plot(density(g$height),main="Goals",xlab="Height",ylab="Density")
lines(x1,dnorm(x1,mean(g$height),sd(g$height)),col="red")
plot(density(s$height),main="Goals",xlab="Height",ylab="Density")
lines(x2,dnorm(x2,mean(s$height),sd(s$height)),col="red")
#Weight
par(mfrow=c(1,2))
x1 = sort(rnorm(length(g$weight),mean(g$weight),sd(g$weight)))
x2 = sort(rnorm(length(s$weight),mean(s$weight),sd(s$weight)))
plot(density(g$weight),main="Goals",xlab="Weight",ylab="Density")
lines(x1,dnorm(x1,mean(g$weight),sd(g$weight)),col="red")
plot(density(s$weight),main="Goals",xlab="Height",ylab="Density")
lines(x2,dnorm(x2,mean(s$weight),sd(s$weight)),col="red")


#____Exploring distributions for Angle
nhl=nhl[,-c(1,3)]
nhl = subset(nhl, Minute > 0 & Minute <=65)
g = subset(nhl, etype == 1)
s = subset(nhl, etype == 0)
g1 = g$angles*-1+90 
s1 = s$angles*-1+90
ga1 = subset(g1, g1 <= 89) #Positive angles
ga2 = subset(g1,g1>89) #Negative angles
sa1 = subset(s1, s1 <= 89)
sa2 = subset(s1, s1 >89)
par(mfrow = c(2,2))
plot(density(ga1), main = "Positive Angle Goals")
plot(density(ga2), main = "Negative Angle Goals")
plot(density(sa1), main = "Positive Angle Shots")
plot(density(sa2), main = "Negative Angle Shots")



#_2016-2017 NHL Goalies
g = read.csv("MissingGoalieBios.csv",header=TRUE)
inds = c(3,4,5,12,15,16,30,33,42,44,52,58,61,63,65,67,68,78,82,83,86,87,92,101,107,108,113,114,119,121,122,126,127,128,131,132,143,148,149,151,154,160,165,170,184,185,188,193)
temp = g[inds,]
#Goalies for app
save(goalie,file="Goalies2016.Rda")

#Coordinates for ice rink
x=c()
for(i in 12:190) {
  x=c(x,rep(i-101,85))
}
y=rep(-42:42,179)
#Coordinates not behind net
coords = data.frame(x,y)
c1 = subset(coords, x<0)
c2 = subset(coords, x>=0)
a1 = atan((89-abs(c1$x))/abs(c1$y))*(180/pi)
d1 = sqrt((89-abs(c1$x))^2 + abs(c1$y)^2)
a2 = atan((89 + c2$x)/abs(c2$y))*(180/pi)
d2 = sqrt((89+c2$x)^2+c2$y^2)
f = data.frame(coords, "Angle"=c(a1,a2),"Distance"=c(d1,d2))
save(f, file="CoordsInFront.Rda")

#Coordinates behind net
x2 = c()
for(i in 1:11) {
  x2 = c(x2, rep(i + 89,85))
}
y2= rep(-42:42,22)
coords2 = data.frame(x2,y2)
c3 = subset(coords2, x2 > 0)
a3 = atan((89 + c3$x)/abs(c3$y))*(180/pi)
d3 = sqrt((89+c3$x)^2+c3$y^2)
c4 = subset(coords2, x2<0)
a4 = atan((abs(c4$x2)-89)/abs(c4$y2))*(180/pi)
d4 = sqrt((abs(c4$x2)-89)^2 + abs(c4$y2)^2)
f2 = data.frame(coords2, "Angle" = c(a4,a3),"Distance" = c(d4,d3))
save(f2,file="CoordsBehind.Rda")

final_coords = rbind(f,f2)
final_coords = arrange(final_coords,x,y)
save(final_coords,file="final_coords.Rda")
