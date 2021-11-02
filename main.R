library(shiny)
library(shinydashboard)
library(ggplot2)
library("RColorBrewer")
library("Hmisc")
library(corrplot)
library(DataExplorer)
library(markdown)
################################
library(dplyr)
library (vioplot)
library(tidyverse)
library(ggcorrplot)
library(caTools)
rsconnect::setAccountInfo(name='5vbfj4-tahi0siham',
                          token='90E22A200C47CA30D06E79040833CC8C',
                          secret='UAnhQpy7l8Gb+kbj9rsF1JS9niBK8MaddXdUASN8')
dataTp <- read_csv('heart.csv')
set.seed(1234)
sample_set <- dataTp %>%
  pull(.) %>% 
  sample.split(SplitRatio = .7)

bankTrain <- subset(dataTp, sample_set == TRUE)
bankTest <- subset(dataTp, sample_set == FALSE)
round(prop.table(table(dataTp$HeartDisease)),3)

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
      menuItem("Reports", tabName = "reports",  icon = icon("bar-chart-o")),
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
              includeMarkdown("www/home.md"), 
              
              fluidRow(
                HTML(paste0(
                  "<br>",
                  " <img style = 'display: block; margin-left: auto; margin-right: auto;' src='descr.png' width = '800'>",
                  "<br>",
                  "<p style = 'text-align: center;'><big><a href='https://www.researchgate.net/figure/The-description-of-Heart-disease-dataset_tbl1_322956561' target='_blank'>The description of Heart disease dataset</a></big></p>",
                  "<br>"
                )),
                
              )),
      tabItem("reports",
              
              
              HTML(paste0(
                "<br>",
                "<h1> Dataset Reports </h1>",
                "<br>",
                "<h3> These reports have been generated with the DataExplorer library, some of them are not visible through this application. Click <a href='https://htmlpreview.github.io/?https://github.com/Sihamtahi/ShinyApp/blob/master/report.html'>here </a> for a better view </h3>",
                
                "<br>"
                
              )),
              tags$iframe(src = './report.html', # put myMarkdown.html to /www
                          width = '100%', height = '800px', 
                          frameborder = 0, scrolling = 'auto'
              )
              #includeHTML("report.html")
      ),
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
      tabItem("prediction",
              
              HTML(paste0(
                "<br>",
                "<h1> Prediction : Logestic Regression  </h1>",
                "</br>"
              )),
              box(
                HTML(paste0(
                  "<br>",
                  "<h1> Correlation Matrix  </h1>",
                  "</br>"
                )),
                
                column(7, plotOutput(outputId = "headerD")),
                width= 10
              ), 
              #box(
              # HTML(paste0(
              #  "<br>",
              #  "<h1> Chi-square Test:  </h1>",
              #  "</br>"
              #  )),
              
              #column(4, textOutput( "chiTable")),
              #width= 10
              # ), 
              box(
                HTML(paste0(
                  "<br>",
                  "<h1> Preprocessing </h1>",
                  
                  "</br>",
                  "<h3>  split the data using a stratified sampling approach. 0.7 for training and 0.3 for testing </h3>",
                  "</br>",
                  "<h3> The table below represent the Class Balancing </h3>"
                )),
                
                column(4, tableOutput(outputId ="preTable")),
                width= 10
              ),
              box(
                HTML(paste0(
                  "<br>",
                  "<h1> Logistic Regression  </h1>"
                  
                  
                )),
                
                column(4, textOutput("text")),
                width= 10
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
                         
                         # Select variable for color
                         selectInput(inputId = "cHis", 
                                     label = "Color with:",
                                     c( "red", "black", "yellow", "green", "seagreen1", "orange"),
                                     selected = "seagreen1"),
                         
                         # Bin width for Histogram
                         sliderInput(inputId = "binsin",
                                     label = "Number of bins for histogram (class interval):",
                                     min = 1,
                                     max = 50,
                                     value = 20), 
                         
                         
                         
                         
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
                column(5, tableOutput(outputId = "centreDisp")), 
                column(7, plotOutput(outputId = "cercle")), 
                width = 25
              ),
              box(
                
                HTML(paste0(
                  "<br>",
                  "<h2> Density (qualitative variables )  </h2>",
                  "</br>"
                )),
                
                #("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBS", "RestingECG",
                #"MaxHR", "ExerciseAngina", "Oldpeak", "ST_Slope", "HeartDisease"), selected = "Age"),
                
                column(3,    
                       fluidRow(
                         selectInput("varDens", "Variable : ", c ("Age", "RestingBP", "Cholesterol", "MaxHR"), selected = "Age"),
                         
                         
                         # Select variable for color
                         selectInput(inputId = "cDens", 
                                     label = "Color with:",
                                     c( "red", "black", "yellow", "green", "seagreen1", "orange"),
                                     selected = "seagreen1"),
                         
                         
                         
                       )
                ),
                column(8,plotOutput(outputId = "densDiag")),
                
                width = 25
              )
              
              
              
      )
      
    )
  )
)
server <- function(input, output){
  
  qualitative_var <- list("Sex",  "ChestPainType", "FastingBS", "RestingECG", "ExerciseAngina", "ST_Slope", "HeartDisease")
  quantitative_var <- list( "Age", "RestingBP",  "Cholesterol",  "MaxHR", "Oldpeak")
  quantitative_var_Dens <- list("Sex",  "ChestPainType", "ExerciseAngina", "ST_Slope")
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
    
    
    
    glimpse(data())
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
    
    if ( is.element(input$varHist, quantitative_var_Dens)  )
    {
      ggplot(data = data()) + 
        geom_bar(mapping = aes(x = 1, fill = data()[[input$varHist]], ), position = "fill") + coord_polar(theta = "y") + labs(fill = input$varHist)
    }
    
    
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
  
  
  output$headerD <- renderPlot({
    
    # header(data)
    # sapply(data, function(x) sum(is.na(x)))
    # summary(dataTp)
    numericVarName <- names(which(sapply(dataTp, is.numeric)))
    corr <- cor(dataTp[,numericVarName], use = 'pairwise.complete.obs')
    ggcorrplot(corr, lab = TRUE)
  })
  output$densDiag <- renderPlot({
    
    ggplot(data()) + geom_density(aes(x = data()[[input$varDens]]), bw = 3, color= input$cDens)
    
    
  })
  logit.mod <- glm(HeartDisease ~., family = binomial(link = 'logit'), data = dataTp)
  logit.pred.prob <- predict(logit.mod, bankTest, type = 'response')
  logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
  head(bankTest,10)
  output$text <-  renderText({
    summary(logit.mod)
    
  })
  output$preTable<- renderTable({
    round(prop.table(table(dataTp$HeartDisease)),3)
  })
  output$chiTable<- renderText({
    chi.square <- vector()
    p.value <- vector()
    cateVar <- dataTp %>% 
      dplyr::select(-HeartDisease) %>% 
      keep(is.factor)
    
    for (i in 1:length(cateVar)) {
      p.value[i] <- chisq.test(dataTp$HeartDisease, unname(unlist(cateVar[i])), correct = FALSE)[3]$p.value
      chi.square[i] <- unname(chisq.test(bankChurn$HeartDisease, unname(unlist(cateVar[i])), correct = FALSE)[1]$statistic)
    }
    
    chi_sqaure_test <- tibble(variable = names(cateVar)) %>% 
      add_column(chi.square = chi.square) %>% 
      add_column(p.value = p.value)
    knitr::kable(chi_sqaure_test)
    
    
  })
  ##############################
  
  
  
}


shinyApp(ui, server)