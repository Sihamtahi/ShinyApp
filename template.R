library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(openintro)
library(plotly)
library(DT)
data(hsb2)

#Shiny Project Paul Jozefek 

#Add CSS styling to descriptions
my_css <- "

#descriptionreg {
color: darkred; font-size: 18px; font-style: bold;}

#descriptionbox {
color: darkred; font-size: 18px; font-style: bold;}

#description {
color: darkred; font-size: 18px; font-style: bold;}

#descriptiontukey {
color: darkred; font-size: 18px; font-style: bold;}
"

# Define UI for application 
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # Add the CSS style to the Shiny app
                tags$style(my_css),
                
                titlePanel("Analysis of the HSB2 Dataset, by Paul Jozefek"),
                
                # Sidebar layout with a input and output definitions 
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for y-axis
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("Reading" = "read", "Writing" ="write", "Math" = "math", "Science" = "science", "Social Studies" = "socst"), 
                                selected = "math"),
                    
                    # Select variable for x-axis
                    selectInput(inputId = "x", 
                                label = "Scatterplot X-axis:",
                                choices = c("Reading" = "read", "Writing" ="write", "Math" = "math", "Science" = "science", "Social Studies" = "socst"), 
                                selected = "write"),
                    
                    # Select variable for w-axis Boxplot
                    selectInput(inputId = "w", 
                                label = "Boxplot X-axis:",
                                choices = c("Gender" = "gender", "Race" = "race", "Socioeconomic Status" = "ses", "School Type" = "schtyp", "Program" = "prog"), 
                                selected = "race"),
                    
                    # Select variable for color
                    selectInput(inputId = "z", 
                                label = "Color by:",
                                choices = c("Gender" = "gender", "Race" = "race", "Socioeconomic Status" = "ses", "School Type" = "schtyp", "Program" = "prog"),
                                selected = "gender"),
                    
                    # Add checkbox for best fit line
                    checkboxInput("fit", "Add best fit line to Scatterplot", TRUE),
                    
                    # Set alpha level
                    sliderInput(inputId = "alpha", 
                                label = "Alpha:", 
                                min = 0, max = 1, 
                                value = 1),
                    
                    # Set point size
                    sliderInput(inputId = "size", 
                                label = "Size:", 
                                min = 0, max = 5, 
                                value = 1),
                    
                    # Bin width for Histogram
                    sliderInput(inputId = "binsin",
                                label = "Number of bins for histogram:",
                                min = 1,
                                max = 50,
                                value = 20),
                    
                    # Built with Shiny by RStudio for Baruch
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       " for ",
                       img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9b/Baruch_logo.svg/2000px-Baruch_logo.svg.png", height = "30px"))
                    
                  ),
                  
                  # Outputs
                  mainPanel(
                    tabsetPanel(type="tabs",
                                
                                #Tab 1 Plots
                                tabPanel(title="Plots",
                                         h3("Boxplot"),    # Third level header: Boxplot
                                         plotOutput(outputId = "boxplot"),
                                         textOutput(outputId = "descriptionbox"),
                                         br(),                 # Single line break for a little bit of visual separation
                                         h3("Scatterplot"),    # Third level header: Scatterplot
                                         plotlyOutput(outputId = "scatterplot"),
                                         textOutput(outputId = "description"),
                                         br(),                 # Single line break for a little bit of visual separation
                                         br(),                 # Single line break for a little bit of visual separation
                                         br()),                 # Single line break for a little bit of visual separation),
                                
                                #Tab 2 Histograms
                                tabPanel(title="Histograms",
                                         h3("Histogram for selected X value"),    # Third level header: Densityplot
                                         plotlyOutput(outputId = "histogramplot"),
                                         br(),                 # Single line break for a little bit of visual separation
                                         h3("Histogram for selected Y value"),    # Third level header: Scatterplot
                                         plotlyOutput(outputId = "histogramplottwo")),
                                
                                #Tab 3 Regression
                                tabPanel(title="Regression",
                                         h3("Summary Statistics and Simple Linear Regression"),    # Third level header: Regression
                                         h4("Using Scatterplot Y and X axis."),    # Fourth level header: Regression
                                         textOutput(outputId = "correlation"),
                                         htmlOutput(outputId = "avgs"),
                                         textOutput(outputId = "descriptionreg"),
                                         verbatimTextOutput(outputId = "lmoutput"),
                                         h3("Regression Plots"),    # Third level header: Regression Plots
                                         plotlyOutput(outputId = "regline"),
                                         plotOutput(outputId = "regplots")),
                                
                                #Tab 4 Tukey Test
                                tabPanel(title="Tukey",
                                         h3("Tukey's HSD Test"), # Third level header: Tukey
                                         verbatimTextOutput(outputId = "tukeyoutput"),
                                         textOutput(outputId = "descriptiontukey")),
                                
                                #Tab 5 Data
                                tabPanel(title="Data Table",
                                         h3("Data table"),     # Third level header: Data table
                                         h5("Click the button Show (on left) to see the data by your specified number of rows"),     # Fifth level header
                                         DT::dataTableOutput("datatable"))
                                
                    )
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create boxplot object the plotOutput function is expecting
  output$boxplot <- renderPlot({
    ggplot(data = hsb2, aes_string(x = input$w, y = input$y, color=input$z)) +
      geom_boxplot(fill="peachpuff")+theme_minimal()
  })
  
  # Create descriptive text
  output$descriptionbox <- renderText({
    paste0("The plot above visualizes the relationship between ", 
           input$w, " and ", input$y, ", conditional on ", input$z, ".")
  })
  
  # Create scatterplot object the plotlyOutput function is expecting
  output$scatterplot <- renderPlotly({
    ggplotly({
      p <- ggplot(data = hsb2, aes_string(x = input$x, y = input$y, color=input$z)) +
        geom_point(alpha = input$alpha, size = input$size)+theme_minimal()
      
      #if check box selected plot best fit line
      if (input$fit) {
        p <- p + geom_smooth(method = "lm")
      }
      p
      
    })
  })
  
  # Create descriptive text
  output$description <- renderText({
    paste0("The plot above visualizes the relationship between ", 
           input$x, " and ", input$y, ", conditional on ", input$z, ".")
  })
  
  # Create histogram
  output$histogramplot <- renderPlotly({
    ggplotly({
      ggplot(data = hsb2, aes_string(x = input$x)) +
        geom_histogram(bins=input$binsin, color="black", fill="seagreen1")+theme_minimal()
    })
  })
  
  # Create histogram2
  output$histogramplottwo <- renderPlotly({
    ggplotly({
      ggplot(data = hsb2, aes_string(x = input$y)) +
        geom_histogram(bins=input$binsin, color="black", fill="plum1")+theme_minimal()
    }) 
  })
  
  # Create text output stating the correlation between the two ploted 
  output$correlation <- renderText({
    r <- round(cor(hsb2[, input$x], hsb2[, input$y], use = "pairwise"), 3)
    paste0("Correlation between ",input$x, " and ", input$y, " = ", r, ".")
  })
  
  # Calculate averages
  output$avgs <- renderUI({
    avg_x <- hsb2 %>% pull(input$x) %>% mean() %>% round(2)
    avg_y <- hsb2 %>% pull(input$y) %>% mean() %>% round(2)
    HTML(
      paste("Average", input$x, "=", avg_x),
      "<br/>",
      paste("Average", input$y, "=", avg_y)
    )
  })
  
  # Create descriptive text
  output$descriptionreg <- renderText({
    paste0("The regression below is between the dependent variable ", 
           input$y, " and the independent variable ", input$x, "")
  })  
  
  # Create regression output
  output$lmoutput <- renderPrint({
    x <- hsb2 %>% pull(input$x)
    y <- hsb2 %>% pull(input$y)
    print(summary(lm(y ~ x, data = hsb2)), digits = 3, signif.stars = FALSE)
  })
  
  # Create regression line
  output$regline <- renderPlotly({
    ggplotly({
      ggplot(data = hsb2, aes_string(x = input$x, y = input$y)) +
        geom_point(alpha = input$alpha, size = input$size, color="red")+
        geom_smooth(method='lm')+theme_minimal()
    })
  })
  
  # Create regression plots
  output$regplots <- renderPlot({
    x <- hsb2 %>% pull(input$x)
    y <- hsb2 %>% pull(input$y)
    par(mfrow=c(2,2))
    plot(lm(y ~ x, data = hsb2))
    
  })
  
  # Create Tukey output
  output$tukeyoutput <- renderPrint({
    y <- hsb2 %>% pull(input$y)
    w <- hsb2 %>% pull(input$w)
    print(TukeyHSD(aov(y ~ w, data = hsb2)))
  })
  
  # Create descriptive text
  output$descriptiontukey <- renderText({
    paste0("The Tukey test helps determine if there's a statistically significant
           difference in ", input$y, " scores among the category ", input$w, "")
  })
  
  # Output data table
  output$datatable <- DT::renderDataTable({
    hsb2
    
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)