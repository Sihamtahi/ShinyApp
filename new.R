library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggcorrplot)
library("RColorBrewer")
library("Hmisc")
library(corrplot)


ui<- dashboardPage(
  
  dashboardHeader(title = "ShinyHeart ❤️", titleWidth = 230),
  
  dashboardSidebar(
    
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://archive.ics.uci.edu/ml/datasets/heart+disease' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='search2.png' width = '186'></a>",
        "<br>",
        "<p style = 'text-align: center;'><small><a href='https://www.researchgate.net/figure/The-description-of-Heart-disease-dataset_tbl1_322956561' target='_blank'>Heart Diseases Detection</a></small></p>",
        "<br>"
      )),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Loading Data", tabName = "loading", icon = icon("table")),
      menuItem("Visualisations Univariable", tabName = "visualisationu", icon = icon("stats", lib = "glyphicon") ),
      menuItem("Visualisations Bivariable", tabName = "visalisationb", icon = icon("stats", lib = "glyphicon") ),
      menuItem("Prediction", tabName = "prediction",  icon = icon("tasks")),
      
      
      
      
      HTML(paste0(
        "<br><br><br><br><br><br><br><br><br>",
        
        "<br>"),
        HTML(paste0(
          "<script>",
          "var today = new Date();",
          "var yyyy = today.getFullYear();",
          "</script>",
          "<p style = 'text-align: center;'><small> <a href='https://flowcv.me/sihamtahi' target='_blank'>Made with ☕  by TAHI SIHAM;</a> - <script>document.write(yyyy);</script></small></p>")
        )
      )
      
      
      
    )),
  
  dashboardBody(
    tabItems(
      tabItem("home",
              #includeMarkdown("www/home.md"), 
              fluidRow(
                HTML(paste0(
                  "<br>",
                  " <img style = 'display: block; margin-left: auto; margin-right: auto;' src='descr.png' width = '800'>",
                  "<br>",
                  "<p style = 'text-align: center;'><big><a href='https://www.researchgate.net/figure/The-description-of-Heart-disease-dataset_tbl1_322956561' target='_blank'>The description of Heart disease dataset</a></big></p>",
                  "<br>"
                )),
                
              )),
      tabItem("visalisationb",
              
              HTML(paste0(
                "<br>",
                "<h1> Bivar Diagrammes  </h1>",
                "<h2> Choose varaibles to display  </h2>",
                "</br>"
              )),
              
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
                ),
                box(
                  
                  HTML(paste0(
                    "<br>",
                    
                    "<h2> Correlation</h2>",
                    "</br>"
                  )),
                  
                  plotOutput("Correlation_tab"), width = 8)
              ),
              
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
              
              HTML(paste0(
                "<br>",
                "<h1> Univar Diagrammes  </h1>",
                "</br>"
              )),
               
                
                box(
                  
                  HTML(paste0(
                    "<br>",
                    "<h2> Distribution  </h2>",
                    "</br>"
                  )),
                  
                 
                         #("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                          #"MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease"), selected = "Age"),
                        
                  column(3,    
                          fluidRow(
                         selectInput("varHist", "Variable :      ", c
                              ("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                                "MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease"), selected = "Age"),
                         
                         selectInput("typeHis", "Variable :      ", c
                                     ("Effectifs", "Cumulated Effectifs")),
                
                           # Bin width for Histogram
                           sliderInput(inputId = "binsin",
                                       label = "Number of bins for histogram (class interval):",
                                       min = 1,
                                       max = 50,
                                       value = 20), 
                         # Select variable for color
                         selectInput(inputId = "cHis", 
                                     label = "Color with:",
                                     c( "red", "black", "yellow", "green", "seagreen1", "orange"),
                                     selected = "seagreen1"),
                         
                         
                         
                         )
                  ),
                  column(8,
                         plotOutput(outputId = "effectifsDiag")),
                  
                  
                  
                  width = 25
                ),
              
              box(
                HTML(paste0(
                  "<br>",
                  "<h2> Calculations related to the selected variable   </h2>",
                  "</br>"
                )),
                column(4, tableOutput(outputId = "centreDisp")), 
                column(4, plotOutput(outputId = "cercle")), 
                width = 25
                ),
              box(
                
                HTML(paste0(
                  "<br>",
                  "<h2> Density   </h2>",
                  "</br>"
                )),
                
                #("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                #"MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease"), selected = "Age"),
                
                column(3,    
                       fluidRow(
                         selectInput("varDens", "Variable :      ", c
                                     ("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                                       "MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease"), selected = "Age"),
                         
                         
                         # Select variable for color
                         selectInput(inputId = "cDens", 
                                     label = "Color with:",
                                     c( "red", "black", "yellow", "green", "seagreen1", "orange"),
                                     selected = "seagreen1"),
                         
                         
                         
                       )
                ),
                column(8,
                       plotOutput(outputId = "densDiag")),
                fluidRow(
                  column(4, plotOutput(outputId = "effectifsCumCurve")),
                  column(4, plotOutput(outputId = "freqCumCurve"))
                ),
                
                fluidRow(
                  column(4, plotOutput(outputId = "effectifsHist")),   
                  column(4, plotOutput(outputId = "effectifHistFreqDens"))
                ), 
                
                
                
                width = 25
              ),
              
               fluidRow(
                
                column(6, plotOutput(outputId = "effectifsCumDiag"))
              ),
              fluidRow(
                column(4,  plotOutput(outputId = "boiteAMoustache"))
                
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
  tabCentreDispQuanti<- reactive ({
    tab <- table(data()[[input$varHist]])
    summary.tmp<- cbind.data.frame(tab, prop.table(tab))
    summary.tmp <- summary.tmp[ ,c(1,2,4)]
   # colnames(summary.tmp) <- c("categories", "counts ", "percentages")
    
  })
  
  tabCentreDisp<- reactive ({
    
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Mediane", "1e quartile", "3e quartile", "Vairance", "Ecart-type" )
    summary.tmp <- c( max( data()[[input$varHist]]),
                      min( data()[[input$varHist]] ),
                      mean( data()[[input$varHist]] ),
                      median( data()[[input$varHist]]),
                      quantile( (data()[[input$varHist]]))[2] ,
                      quantile( (data()[[input$varHist]]))[4],
                      var( data()[[input$varHist]]),
                      sqrt( var(data()[[input$varHist]]) )
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
      ###### X quantitaif et Y quantitatif -> Correlation Simple
      
      if ( is.element(input$Yfeatures, quantitative_var)  )
      {
        plot(data()[[input$Xfeatures]], data()[[input$Yfeatures]], xlab = input$Xfeatures, ylab = input$Yfeatures, 
        )
      }
      ###### X quantitatif et Y qualitatif -> boite parallÃ¨le
      else
      {
        boxplot( data()[[input$Xfeatures]] ~ data()[[input$Yfeatures]], data = data(),  xlab = input$Xfeatures, ylab = input$Yfeatures,
                 horizontal=TRUE, 
                 col=brewer.pal(n = 5, name = "Dark2"))
        
      }
      
    }
    else
    {
      ###### X quaalitatif et Y quantitatif ->  
      
      if ( is.element(input$Yfeatures, quantitative_var)  )
      {
        boxplot( data()[[input$Yfeatures]]~data()[[input$Xfeatures]], data = data(),  xlab = input$Xfeatures, ylab = input$Yfeatures,
                 col=brewer.pal(n = 5, name = "Dark2"), 
                 legend.text = rownames( table(data()[[input$Xfeatures]],data()[[input$Yfeatures]])))
      }
      ###### X qualitatif et Y qualitatif -> diagramme de barre
      else
      {
        tablee<- table(data()[[input$Xfeatures]],data()[[input$Yfeatures]])
        
        barplot(tablee, beside=FALSE, xlab = paste (input$Xfeatures," avec ", input$Yfeatures), 
                ylab = "L'effectifs", 
                las=2 , 
                col=brewer.pal(n = 5, name = "Dark2") , 
                main="",  legend.text = rownames(tablee))
        
        
      }
    }
    
  
  })
  
  output$prediction <- renderPlot({
    
    plot(iris$Sepal.Length, iris[[input$features]], xlab = "X", ylab = "Y")
  })
  
  output$effectifsDiag <- renderPlot({
   # plot(table(data()), col = "green4", xlab= "age", ylab ="Effectifs", main="Distribution des effectifs pour l'age")
    if ( is.element(input$varHist, quantitative_var)  )
      
    { 
      ggplot(data = data(), aes_string(x = input$varHist)) +
        geom_histogram(bins=input$binsin, color="black", fill=input$cHis)+theme_minimal()  + ggtitle("histogram chart")  
    }
    else
    {
      ggplot(data = data(), aes_string(x = input$varHist)) +
        geom_bar( color="black", fill=input$cHis)+theme_minimal() + ggtitle("histogram chart")  
    }
  
    
  })
  
  output$effectifsCumDiag <- renderPlot({
    plot(ecdf(as.numeric(as.character(tabStat()[,1]))), col = "green4", xlab= "age", ylab ="Frequence Cumulees", main="Frequence cumulees pour l'age")
    
  }) 
  
  output$boiteAMoustache <- renderPlot({ 
    boxplot(data() , col= grey(0.8), main= "Ages des salaries", ylab= "Age", las=1)
    rug (data()[,1], side=2)
    
  })
  output$cercle <- renderPlot({ 
    
    EffType = as.vector(table(data()[[input$varHist]]))
    Freq = EffType/length(data()[[input$varHist]])
    
    df = data.frame(type = levels(data()[[input$varHist]]), value = as.vector(Freq))
    g3 = ggplot(df, aes(x = "", y = value, fill = type)) + geom_bar(width = 1, stat = "identity")
    g4 = g3 + coord_polar("y", start = 0)
    #ggplot(data(), aes(x = "", y = data()[[input$varDens]], fill = data()[[input$varDens]])) + geom_bar(width = 1, stat = "identity")
    
    })
  output$Correlation_tab <- renderPlot({ 
    
    ggplot(data(), aes(x = data[["Age"]], fill = data()[["HeartDisease"]])) + geom_density(alpha = .3)
    
    })
  
  output$centreDisp <- renderTable({
    
    if ( is.element(input$varHist, quantitative_var)  )
      
    { 
         
       tabCentreDisp()
    
    }
    else
    {
      tabCentreDispQuanti()
    }
    
    
    })
  
  
  
  output$densDiag <- renderPlot({
    
   ggplot(data()) + geom_density(aes(x = data()[[input$varDens]]), bw = 1)
    
     
     
    
  })
  ##############################
  fecondite <- reactive({
    if(!"fecondite" %in% colnames(data())) return(NULL)
    data()[[input$varDens]]$fecondite 
  })
  
  output$effectifsHist  <- renderPlot({
    hist(data()[[input$varDens]], freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
         main = " Histogramme de l'indice de fecondite ", col = "blue",
         xlab = " Indice de Fecodité ", ylab = " Effectifs ", las =1,
         breaks = seq(0.8, 3, by=0.2), right = FALSE, cex.lab = 1.5)
    
    
  })
  output$effectifHistFreqDens <- renderPlot({
    
    hist(fecondite(), freq= FALSE, cex.axis = 1.5, cex.main = 1.5, 
         main = " Histogramme de l'indice de fecondite ", col = "green",
         xlab = " Indice de FecoditÃ© ", ylab = " DesnitÃ© de frÃ©quence ", las = 1,
         breaks = seq(0.8, 3, by=0.2), right = FALSE, cex.lab= 1.5)
  })
  
  
  
  
  
  output$effectifsCumCurve <-  renderPlot({
    tmp.hist <- hist(fecondite(), plot= FALSE, breaks= seq(0.8, 3, by=0.2), right= FALSE)
    
    plot(x= tmp.hist$breaks[-1],y= cumsum(tmp.hist$counts), 
         xlab= "FeconditÃ© (borne sup. de chaque classe)",
         ylab = "Effectifs cumules ", cex.axis = 1.5, cex.lab= 1.5, 
         main = "Courbe cumulative de l'indice fecondite", 
         type = "o", col= "blue", lwd= 2, cex.main = 1.5)
    
  })
  
  
  
  output$freqCumCurve <-  renderPlot({
    tmp.hist <- hist(fecondite(), plot= FALSE, breaks= seq(0.8, 3, by=0.2), right= FALSE)
    
    plot(x= tmp.hist$breaks[-1],y= cumsum(tmp.hist$density * rep(0.2, 11)), 
         xlab= "FeconditÃ© (borne sup. de chaque classe)",
         ylab = "Effectifs cumules ", cex.axis = 1.5, cex.lab= 1.5, 
         main = "Courbe cumulative de l'indice fecondite", 
         type = "o", col= "green", lwd= 2, cex.main = 1.5)
    
  })
  
  
  
}


shinyApp(ui, server)