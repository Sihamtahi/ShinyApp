library(shiny)
library(shinydashboard)
library(ggplot2)

ui<- fluidPage( 
  
  fluidRow(
    column(3, fileInput(inputId="file1", label="choose  CSV file", accept=c("text/plain", ".csv")) ),
    column(4, verbatimTextOutput(outputId = "summary"))
  ),
  
  fluidRow(
    column(1, actionButton(inputId = "go", label = "Load")),
    column(6, offset = 1, tableOutput(outputId= "contents")) 
    
  )
)


server <- function (input, output)
{ 
  
  data <- eventReactive(input$go,
                        {
                          inFile<- input$file1
                          if(is.null(inFile)) return (NULL)
                          read.csv(inFile$datapath, header= FALSE)
                        }
  )
  output$contents <- renderTable({data()})
  
  
  
  
  
}


shinyApp(ui = ui, server= server)