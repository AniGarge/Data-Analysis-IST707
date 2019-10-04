library(shiny)

rsconnect::setAccountInfo(name='skjain',
                          token='2D1F94B839F9E204BAB3E6B0868D59ED',
                          secret='W67nRkOZjcyAXQE6Y706JCP1uzWyzQuH+jZIxTJd')

shinyUI(fluidPage(
  
  titlePanel("Employee Attrition"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("support", "Support",
                  min = 0, max = 1, step = 0.1,value = 0.5
                  ),
      
      sliderInput("confidence", "Confidence",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1)
      
      
    ),
    
    
    mainPanel(
      plotOutput("myPlot")
    )
    
  )
)
)

