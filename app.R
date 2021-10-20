library(shiny)
library(shinydashboard)
library(ggplot2)

ui<- dashboardPage(
  
  dashboardHeader(title = "Projet R shiny"),
  
  
  
  
  dashboardSidebar(),
  
  
  
  
  
  dashboardBody(
    
    box(plotOutput("Correlation_Plot"), width = 8)
  )
)
server <- function(input, output){
  
  output$myPlot = renderPlot({
    hist(rnorm(1000))
  })
  output$Correlation_Plot <- renderPlot({
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
  })
  
}
shinyApp(ui, server)