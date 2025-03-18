library(shiny)

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Prediction", 
  titlePanel("Predicted Probabilities of an NHL Goal"),
  sidebarLayout(
    sidebarPanel(
  sliderInput(inputId = "minute",label="Minute",min=0,max=65,value=0),
  uiOutput(outputId = "goalie"),
  uiOutput(outputId = "shoot"),
  uiOutput(outputId = "type"),
  uiOutput(outputId = "home"),
  uiOutput(outputId = "man"),
  uiOutput(outputId = "playoff"),
  textOutput("t")
    ),
    mainPanel(
      plotOutput("probs"),
      plotOutput("probs2")
    )
  )
   )
)
)
)