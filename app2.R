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
    
  ),
  
  
  fluidRow(
    column(4, offset = 1, tableOutput(outputId= "tabcontents")),
    column(4,  plotOutput(outputId = "boiteAMoustache"))
  ),


  fluidRow(
  column(6, plotOutput(outputId = "effectifsDiag")),
  column(6, plotOutput(outputId = "effectifsCumDiag"))
  ),
  
  fluidRow(
    column(6,  plotOutput(outputId = "boiteAMoustache")),
    column(4,offset = 2,   plotOutput(outputId = "centreDisp"))
   
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
  
  
  tabStat <- reactive ({
    
    table.tmp <- as.data.frame(table( data() ))
    
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    table.tmp<- cbind(table.tmp, 
                      table.tmp[[2]]/nrow( data() )*100,
                      table.tmp[[3]]/nrow(data())*100)
    
    
    colnames(table.tmp) <- c("Ages", "Effectifs", "Effectifs Cum.", "Frequences", "Frequences Cum.")
    
    
    table.tmp
      
   })
  
  
  tabCentreDisp<- reactive ({
    
    
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Mediane", "1e quartile", "3e quartile", "Vairance", "Ecart-type" )
    
    
    summary.tmp <- c( max( data()[,1]),
                      min( data()[,1] ),
                      mean( data()[,1] ),
                      median( data()[,1] ),
                      quantile( (data()[,1]))[2] ,
                      quantile( (data()[,1]))[4],
                      var( data()[,1]),
                      sqrt( var(data()[,1]) )
    )
    
    summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
    colnames(summary.tmp) <- c("Caratcteristiques", "Valeur")
    
    summary.tmp
  })
  output$contents <- renderTable({data()})
  output$tabcontents <- renderTable({tabStat()})
  
  
  
  output$effectifsDiag <- renderPlot({
    plot(table(data()), col = "green4", xlab= "age", ylab ="Effectifs", main="Distribution des effectifs pour l'age")
    
  })
  
  output$effectifsCumDiag <- renderPlot({
    plot(ecdf(as.numeric(as.character(tabStat()[,1]))), col = "green4", xlab= "age", ylab ="Frequence Cumulees", main="Frequence cumulees pour l'age")
    
  }) 
  
  output$boiteAMoustache <- renderPlot({ 
    boxplot(data() , col= grey(0.8), main= "Ages des salaries", ylab= "Age", las=1)
    rug (data()[,1], side=2)
    
  })
  
  
  output$summary<- renderPrint({
    t(summary(data())) })

   
   output$centreDisp <- renderTable({tabCentreDisp()})
   
}


 
 shinyApp(ui = ui, server= server)