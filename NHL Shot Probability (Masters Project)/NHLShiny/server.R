library(shiny)
library(ggplot2)
load("ProjectData3.Rda")
nhl = subset(nhl, Minute > 0 & Minute <=65)
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

load(file="final_coords.Rda",verbose = T)
final_coords = subset(final_coords, (x >-90 & x < 0))
final_coords = subset(final_coords, !(((y %in% c(-4:4)) & (x %in% c(-90:-85))) | (x==-84 & y%in%c(-3:3)) | (x==-83 & y%in%c(-1:1))))
load(file="Goalies2016.Rda",verbose=T)
goalie$Name = as.character(goalie$Name)
goalie$catch = as.character(goalie$catch)
library(jpeg)
img <- readJPEG("t.jpg")
g=as.raster(img)
#ggplot(final_coords, aes(x, y))+annotation_raster(g, xmin=-100,xmax=100,ymin=-42,ymax=42.5)+ geom_tile(aes(fill = top/(top+bot), alpha=1)) +scale_fill_gradient(low = "red", high = "green")+scale_alpha_continuous(range=c(0.7,.9))

empirical_probs = function(minute = 0, shooter = " ", type = " ", home = " ", manpower = " ", playoffs = " ", height = NULL, weight = NULL, catch = NULL) {
  t = final_coords$top*ifelse(minute == 0,1, g_min(minute))*ifelse(shooter==" ",1, g_shoot[shooter])*ifelse(type == " ",1, g_type[type])*ifelse(home == " ",1, g_home[1+as.numeric(home)])*ifelse(manpower == " ",1, g_man[manpower])*ifelse(playoffs == " ",1, g_play[1+as.numeric(playoffs)])*ifelse(is.null(height),1, g_height(height))*ifelse(is.null(weight),1, g_weight(weight))*ifelse(is.null(catch),1, g_catch[catch]);
  b = final_coords$bot*ifelse(minute == 0,1, s_min(minute))*ifelse(shooter== " ",1, s_shoot[shooter])*ifelse(type == " ",1, s_type[type])*ifelse(home == " ",1, s_home[1+as.numeric(home)])*ifelse(manpower == " ",1, s_man[manpower])*ifelse(playoffs == " ",1, s_play[1+as.numeric(playoffs)])*ifelse(is.null(height),1, s_height(height))*ifelse(is.null(weight),1, s_weight(weight))*ifelse(is.null(catch),1, s_catch[catch]);
  return(t/(t+b))
}


shinyServer(function(input, output) {
  output$goalie = renderUI(selectizeInput("Goalie", "Goalie", choices = c(" ",unique(goalie$Name2))));
  output$shoot = renderUI(selectInput("Shoot","Shooter Position", choices = c(" ",'F','D')));
  output$type = renderUI(selectInput("Type","Shot Type", choices = c(" ",sort(as.character(unique(nhl$type))))));
  output$home = renderUI(selectInput("Home","Home/Away", choices = c(" ",1,0)));
  output$man = renderUI(selectInput("Man","Manpower",choices = c(" ",sort(as.character(unique(nhl$manpower))))));
  output$playoff = renderUI(selectInput("Playoff", "Reg/Playoff", choices = c(" ",1,0)))
  

  current_goalie <- reactive({
    if(input$Goalie == " "){
      data.frame()
      } else {
        subset(goalie, Name2 == input$Goalie)
      }
  })
  
  current_probs <- reactive({
     empirical_probs(minute = input$minute, shooter = input$Shoot, type = input$Type, home = input$Home, manpower = input$Man, playoffs = input$Playoff, height = current_goalie()$ht, weight = current_goalie()$wt, catch = current_goalie()$catch);
  })
  output$probs=renderPlot({
    ggplot(final_coords, aes(x, y))+annotation_raster(g, xmin=-100,xmax=100,ymin=-42,ymax=42.5)+ geom_tile(show.legend = F,aes(fill = current_probs(), alpha=1)) +scale_fill_distiller(palette = "Spectral",limits=c(0,.7),name = "Estimated\nprobability\nof goal")+scale_alpha_continuous(range=c(0.7,.9))+xlab("")+ylab("")+theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
  }
  )
  
  output$probs2 = renderPlot({
    ggplot(data.frame(current_probs()), aes(x = current_probs(),fill="red")) + geom_density() + xlab("Current probabilities") + ylab("Density") + ggtitle("")
  })
  
  
  output$t = renderText({
    ifelse(length(current_goalie()) == 0, print(""), paste("Height: ", current_goalie()$ht,"Weight: ",current_goalie()$wt,"Catch: ",current_goalie()$catch, max(current_probs())))
  })
  
  output$image <- renderImage({
    list(src = 'i.png')
  })

  })

