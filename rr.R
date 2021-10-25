library(shiny)
library(shinydashboard)
library(ggplot2)

ui<- dashboardPage(
  
  dashboardHeader(title = "Projet R shiny", titleWidth = 230),
  
  dashboardSidebar(
    
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='tel.png - black.png' width = '186'></a>",
        "<br>",
        "<p style = 'text-align: center;'><small><a href='https://www.nps.gov/subjects/hfc/arrowhead-artwork.htm' target='_blank'>NPS logo disclaimer</a></small></p>",
        "<br>"
      )),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Loading Data", tabName = "loading", icon = icon("table")),
      menuItem("Visualisations Univariable", tabName = "visualisationu", icon = icon("stats", lib = "glyphicon") ),
      menuItem("Visualisations Bivariable", tabName = "visalisationb", icon = icon("stats", lib = "glyphicon") ),
      menuItem("Prediction", tabName = "prediction",  icon = icon("tasks")),
      
      
      
      
      HTML(paste0(
        "<br><br><br><br><br><br><br><br><br>",
        "<table style='margin-left:auto; margin-right:auto;'>",
        "<tr>",
        "<td style='padding: 5px;'><a href='https://www.facebook.com/nationalparkservice' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.twitter.com/natlparkservice' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
        "</tr>",
        "</table>",
        "<br>"),
        
        
        HTML(paste0(
          "<script>",
          "var today = new Date();",
          "var yyyy = today.getFullYear();",
          "</script>",
          "<p style = 'text-align: center;'><small>&copy; - <a href='https://alessiobenedetti.com' target='_blank'>alessiobenedetti.com</a> - <script>document.write(yyyy);</script></small></p>")
        )
      )
      
      
      
    )),
  
  dashboardBody(
    tabItems(
      tabItem("visalisationb",
              
             
              box(plotOutput("Correlation_Plot"), width = 8),
              fluidRow(
               box(
                selectInput("Xfeatures", " X Features :", c
                            ("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                              "MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease")), width = 4
              ),
              box(
                selectInput("Yfeatures", " Y Features :", c
                            ("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                              "MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease")), width = 4
              )
              )
      ),
      
      tabItem("loading", 
              fluidPage(
                fluidRow(
                  column(2, fileInput(inputId="file1", label="choose  CSV file", accept=c("text/plain", ".csv")) ),
                  column(6, verbatimTextOutput(outputId = "summary"))
                ),
                
                fluidRow(
                  column(1, actionButton(inputId = "go", label = "Load")),
                  column(8, offset = 1, tableOutput(outputId= "contents")) 
                  
                )
                
                
              ) 
      ),
      tabItem("visualisationu",
              fluidRow(
                column(6, plotOutput(outputId = "effectifsDiag")),
                column(6, plotOutput(outputId = "effectifsCumDiag"))
              ),
              fluidRow(
                column(4,  plotOutput(outputId = "boiteAMoustache"))
                
              ),
              fluidRow(
                
                column(4,offset = 2,   plotOutput(outputId = "centreDisp"))
                
              ), 
              
              
              fluidRow(
                column(4, plotOutput(outputId = "effectifsCumCurve")),
                column(4, plotOutput(outputId = "freqCumCurve"))
              ),
              
              fluidRow(
                column(4, plotOutput(outputId = "effectifsHist")),   
                column(4, plotOutput(outputId = "effectifHistFreqDens"))
              )
              
              
              
      )
      
    )
  )
)
server <- function(input, output){
  
  qualitative_var <- list("Sex",  "ChestPainType", "FastingBS", "RestingECG", "ExerciseAngina", "ST_Slope", "HeartDisease")
  quantitative_var <- list( "Age", "RestingBP",  "Cholesterol",  "MaxHR", "Oldpeak")
  ##############################################################################################
  
  output$summary<- renderPrint({
    t(summary(data())) })
  
  data <- eventReactive(input$go,
                        {
                          inFile<- input$file1
                          if(is.null(inFile)) return (NULL)
                          read.csv(inFile$datapath, header= TRUE)
                        }
  )
  
  output$contents <- renderTable({data()})
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
  #############################################################################################################
  output$myPlot = renderPlot({
    hist(rnorm(1000))
  })
  
  output$Correlation_Plot <- renderPlot({
    
    
    
    if ( is.element(input$Xfeatures, quantitative_var)  )
      
    { 
      ###### X qualitatif et Y quantitatif -> Correlation Simple
      
      if ( is.element(input$Yfeatures, quantitative_var)  )
      {
        plot(data()[[input$Xfeatures]], data()[[input$Yfeatures]], xlab = input$Xfeatures, ylab = input$Yfeatures, 
             col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)))
      }
      ###### X quantitatif et Y qualitatif -> boite parallèle
      else
      {
        boxplot( data()[[input$Yfeatures]]~data()[[input$Xfeatures]], data = data(),  xlab = input$Xfeatures, ylab = input$Yfeatures,
                 horizontal=TRUE, 
                 col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)))
      }
        
    }
    else
    {
      ###### X quaalitatif et Y quantitatif ->  
      
      if ( is.element(input$Yfeatures, quantitative_var)  )
      {
        boxplot( data()[[input$Yfeatures]]~data()[[input$Xfeatures]], data = data(),  xlab = input$Xfeatures, ylab = input$Yfeatures,
                 col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)))
      }
      ###### X quaalitatif et Y qualitatif -> diagramme de barre
      else
      {
        barplot(table(data()[[input$Xfeatures]],data()[[input$Yfeatures]]), beside=TRUE, xlab = paste (input$Xfeatures," avec ", input$Yfeatures), 
                ylab = "L'effectifs", 
                las=2 , 
                col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                main="")
        
      }
    }
    
  
    
    
    
    
    })
  
  output$prediction <- renderPlot({
    
    plot(iris$Sepal.Length, iris[[input$features]], xlab = "X", ylab = "Y")
  })
  
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
  
  output$centreDisp <- renderTable({tabCentreDisp()})
  ##############################
  fecondite <- reactive({
    if(!"fecondite" %in% colnames(data())) return(NULL)
    data()$fecondite 
  })
  
  output$effectifsHist  <- renderPlot({
    hist(fecondite(), freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
         main = " Histogramme de l'indice de fecondite ", col = "blue",
         xlab = " Indice de Fecodité ", ylab = " Effectifs ", las =1,
         breaks = seq(0.8, 3, by=0.2), right = FALSE, cex.lab = 1.5)
    
    
  })
  output$effectifHistFreqDens <- renderPlot({
    
    hist(fecondite(), freq= FALSE, cex.axis = 1.5, cex.main = 1.5, 
         main = " Histogramme de l'indice de fecondite ", col = "green",
         xlab = " Indice de Fecodité ", ylab = " Desnité de fréquence ", las = 1,
         breaks = seq(0.8, 3, by=0.2), right = FALSE, cex.lab= 1.5)
  })
  
  
  
  
  
  output$effectifsCumCurve <-  renderPlot({
    tmp.hist <- hist(fecondite(), plot= FALSE, breaks= seq(0.8, 3, by=0.2), right= FALSE)
    
    plot(x= tmp.hist$breaks[-1],y= cumsum(tmp.hist$counts), 
         xlab= "Fecondité (borne sup. de chaque classe)",
         ylab = "Effectifs cumules ", cex.axis = 1.5, cex.lab= 1.5, 
         main = "Courbe cumulative de l'indice fecondite", 
         type = "o", col= "blue", lwd= 2, cex.main = 1.5)
    
  })
  
  
  
  output$freqCumCurve <-  renderPlot({
    tmp.hist <- hist(fecondite(), plot= FALSE, breaks= seq(0.8, 3, by=0.2), right= FALSE)
    
    plot(x= tmp.hist$breaks[-1],y= cumsum(tmp.hist$density * rep(0.2, 11)), 
         xlab= "Fecondité (borne sup. de chaque classe)",
         ylab = "Effectifs cumules ", cex.axis = 1.5, cex.lab= 1.5, 
         main = "Courbe cumulative de l'indice fecondite", 
         type = "o", col= "green", lwd= 2, cex.main = 1.5)
    
  })
  
  
  
}


shinyApp(ui, server)