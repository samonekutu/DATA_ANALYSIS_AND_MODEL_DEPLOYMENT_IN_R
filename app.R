#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
modelLR = readRDS("model.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Fish Escape Cause Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Season", "Select Season", c("Winter", "Summer", "Spring","Autumn")),
      selectInput("Species", "Select Species", c("Other","Salmon", "Salmon.Brood", "Salmon.Fresh")),
      sliderInput("Number","Select Number:",min = 1,max = 336470,value = 13536),
      sliderInput("Cu","Enter  level of Copper compounds (Cu):",min = -3.9100,max = 8.4502,value = 1.7553), # 1.7553 is median
      sliderInput("N","Enter  level of Nitrogen compounds (N):",min = -105.5,max = 696.5,value = 358.3),
      sliderInput("P","Enter  level of Phosphorus compounds (P):",min = -13.97 ,max = 244.29,value = 121.98),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h3("Prediction:"), # tags$p creates a paragraph of text h4 header
      tags$h4(" ~~~~~~~~~~~~~~~~~~~~~~~~~\n"),
      textOutput("modelresult"),
      tags$h4(" ~~~~~~~~~~~~~~~~~~~~~~~~~\n"),
      tags$h4(" Automated Predictor Discliamer"),
      tags$h6(" predictor accuracy is about 70%"),
      tags$h6("The original data source used to build model was acquired from Scotland's aquaculture."),
      tags$h6("\n As direct implication of the terms and conditions of data source the Predictor model implicitly inherits the Disclaimer below: \n"),
      tags$h6("'Content is for guidance only and may not be relevant to Your particular circumstances. We do not guarantee that the Contents will always be complete, up to date or relevant (except as required by law)'\n"),
      tags$h6("The Predictor  Model is open to Peer Review Process."),
      tags$h6("copyright 2022")
    )
  )
)
# Define server Number,Cu,Age,N,P,Org,SLR 
server <- function(input, output) {
  
  output$modelresult <- renderText({
    a = input$Season
    b = input$Species
    c = input$Number
    d = input$Cu
    e = input$N
    f = input$P
    newData= data.frame(Season.Winter=ifelse(a == "Winter", 1,0),Species.Other=ifelse(b == "Other",1,0), Number=c ,Cu=d , N=e, P=f)
    
    if (predict(modelLR, newData)=='0'){return("Probably Human related Causes")}else{return("Probably Natural Causes")}
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
