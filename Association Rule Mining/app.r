#install.packages("plyr")
library(plyr)
library(shiny)
library(rsconnect)
library(arules)
library(arulesViz)



    ui <- fluidPage(
      
      # Application title
      titlePanel("Association Rule Mining"),
      
      
      #side bar layout with inp/op
      sidebarLayout(
        sidebarPanel(
          
          #slider input1
          sliderInput(inputId = "support",
                      label="Value of Support",
                      min=0.01,
                      max=1,
                      step = 0.01,
                      value = 0.1),
          #slider input 2
          sliderInput(inputId = "confidence",
                      label = "Value of confidence",
                      min=0.1,
                      max=1,
                      step=0.01,
                      
                      value=0.5)
         ),
        #main panel for output
        mainPanel(
          verbatimTextOutput("Rules"),
          plotOutput("plot_rules")
        )
        
      )
    )
  
  
  server <- function(input, output) { 
    
    
    output$plot_rules<-renderPlot({
      
    rules<-apriori(attrition_data,parameter = list(support = input$support, confidence = input$confidence))
    plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=0)
        
        
  })
  }
  
  shinyApp(ui = ui, server = server)
  
