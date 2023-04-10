
library(dplyr)
library(psych)
library(shiny)
library(shinythemes)
library(DT)
source("./utils/train-model.R")
source("./utils/test-model.R")
source("./utils/load-model.R")
source("./utils/visualization.R")
source("./utils/prediction.R")
source("./utils/find-subregion-belong.R")
source("./utils/correlation.R")
source("./utils/summary.R")
source("./utils/try-examples.R")
options(scipen=9999)

dir <- getwd()

# Define UI for the app ----
ui <- fluidPage(
  
  # general style configuration for UI ----
  tags$style(type='text/css', 
             '#result {white-space: pre-wrap;}',
             '#message {white-space: pre-wrap;}',
             HTML('* {font-family: "Arial";}'),
             '#description {font-size:13px;}',
             '#longitude {vertical-align: middle;height: 34px;}',
             '#latitude {vertical-align: middle;height: 34px;}',
             '#progress {vertical-align: middle;height: 34px;}',
             '#show {padding: 5px;width: 100px;}',
             '#train {padding: 5px;width: 60px;}',),
  
  # UI for navigation bar, includes three tabs ----
  navbarPage(
    
    theme = shinytheme("sandstone"), collapsible = TRUE,
    
    title=span("California Housing Prices Prediction"),
    id="nav",
    position="static-top",
    
    # (1) UI for Introduction tab
    tabPanel("About",
             
             fluidRow(
               h3(strong("Background"), style = "text-align: left"),
               h4("Housing is considered one of the most fundamental needs that one person aspires to own in their life. This is not an easy goal for everyone, many people have to work 10-15 years or even more to save enough money to buy one. Normally, a transaction is made based on the negotiation between the buyer and an agent. An agent is always seeking to sell at a high value to gain greater commission, therefore, if a buyer does not analyze the house thoroughly, the buyer can easily be scammed.",
                  style = "font-size:15px;"),
               h4("The introduced App can be used as a tool to assist buyers in evaluating the value of a house based on multiple variables and recorded values of other houses in the same area.",
                  style = "font-size:15px;"),
               h4("In this prototype, the California Housing dataset was used and can be downloaded from",
                  a("SOURCE", 
                    href = "https://github.com/ageron/handson-ml2/raw/master/datasets/housing/"),
                  ". The data collected information on 10 variables using all the block groups (20,640 observations) in California from the 1990 Census. In this sample, a block group typically has a population of 600 to 3,000 people. In which, the dependent variable is median house value and the description of all variables can be viewed here:",
                  style = "font-size:15px;"),
               tableOutput("description"),
               h3(strong("Instructions"), style = "text-align: left"),
               h4("The App contains 3 sections:", style = "font-size:15px;"),
               h4("1. About: This section provides the background of application and descriptions on data source.", style = "font-size:15px;"),
               h4("2. Train Model: This section allows users to select cluster method to train the model.", style = "font-size:15px;"),
               h4("3. Search: This section allows users to search information of a specific location based on longitude and latitude inputs and view the estimated house value in the area using trained model.", style = "font-size:15px;"),
             )
    ),
    
    # (2) UI for Train Model tab ----
    tabPanel("Train Model",
             
             fluidRow(
               
               column(6,
                # Input: Select a method for clustering ----
                selectInput("options", "Cluster by:",
                            choices = c("Choose one" = "",
                                        "no cluster",
                                        "ocean_proximity", 
                                        "longitude & latitude (using KMeans)")),
                
                # Input: Specify number of clusters ----
                sliderInput("no_clusters", "Number of clusters:", value = 2,
                             min = 1, max = 30),
                
                # Input: Progress status display ----
                textInput("progress", "Progress:", value = "No need to input" ),
                
                # Include clarifying notes ----
                helpText("Note:", style = "font-size:13px;"),
                helpText("(1) Option \"no cluster\" only allows clusters number set to one."),
                helpText("(2) Option \"ocean_proximity\" only allows clusters number set to four."),
                helpText("(3) As program starts, trained model parameters are loaded and the current model's MAPE is shown. 
                         In case, the current model is not your choice, you can re-train another one."),
                
                # Input: Button to start training model ----
                actionButton("train", "Train"),
                
                # Output: Display training result ----
                h4(""),
                verbatimTextOutput("message"),
               ),
               
               column(6,
                      # Output: Visualize current model's MAPE and MAPE by cluster ----
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotOutput("accuracy", height = "480px")))
               ),
          ),
    ),
    
    # (3) UI for Search tab ----
    tabPanel("Search",
             
             fluidRow(
               
               column(6,
                 # Input: Specify looked-up longitude ----
                 textInput(inputId = "longitude", label = "Longitude:", value = NA),
                 
                 # Input: Specify looked-up latitude ----
                 textInput(inputId = "latitude", label = "Latitude:", value = NA),
                 
                 # Include clarifying notes ----
                 helpText("Note:"),
                 helpText("(1) The program only shows results for longitude and latitude from our data source, some examples can be viewed in Try tab."),
                 helpText("(2) The map plot shows all block locations by cluster and able to highlight the searched region based on longitude and latitude inputs."),
                 helpText("(3) The value range plot can highlight the group that contains searched blocks located at given longitude and latitude."),
                 helpText("(4) The Correlation tab shows the correlation result for cluster that covers longitude and latitude inputs."),
                 
                 # Input: Button to show prediction result ----
                 actionButton("show", "Show Result"),
                 
                 # Output: Display prediction result ----
                 h4(""),
                 verbatimTextOutput("result"),
               ),
               
               column(6,
                  # Output: Tabset w/ Plot, Correlation, Summary and Try ----
                  tabsetPanel(type = "tabs",
                              tabPanel("Plot", plotOutput("plot", 
                                                          height = "480px",
                                                          width = "1160px"),
                                       style = "overflow-x: scroll;"),
                              tabPanel("Correlation", tableOutput("correlation")),
                              tabPanel("Summary", tableOutput("summary")),
                              tabPanel("Try", DT::dataTableOutput("try"),
                                       style = 
                                       "height:400px;
                                       overflow-y: scroll;overflow-x: scroll;")
                              ),
                ),
            ),
        ),
    ),
)

# Define server logic for the app ----
server <- function(input, output, session) {
  
  output$description <- renderTable({
    data.frame(
      variable = c("median_house_value", 
                   "longitude", "latitude", "housing_median_age", 
                   "total_rooms", "total_bedrooms", "population", 
                   "households", "median_income", "ocean_proximity"),
      description = c("Median house value for households within a block (measured in US Dollars)",
                      "A measure of how far west a house is; a more negative value is farther west",
                      "A measure of how far north a house is; a higher value is farther north",
                      "Median age of a house within a block; a lower number is a newer building",
                      "Total number of rooms within a block",
                      "Total number of bedrooms within a block",
                      "Total number of people residing within a block",
                      "Total number of households, a group of people residing within a home unit, for a block",
                      "Median income for households within a block of houses (measured in 10,000s of US Dollars)",
                      "Indicates (very roughly) whether each block group is near the ocean, near the Bay area, inland or on an island.")
    )
  })
  
  # Retrieve longitude input ----
  extractLong <- eventReactive(input$show, {
    input$longitude
  }, ignoreNULL = FALSE)
  
  # Retrieve latitude input ----
  extractLat <- eventReactive(input$show, {
    input$latitude
  }, ignoreNULL = FALSE)
  
  # Retrieve Train command ----
  extractClusterBy <- eventReactive(input$train, {
    input$options
  }, ignoreNULL = FALSE)
  
  # Fix clusters number is 1 in case Clustering by "no cluster" is selected. ----
  observeEvent(input$options, {
    if (input$options == "no cluster") {
      value <- 1
      updateSliderInput(session, "no_clusters", value = value,
                        min = 1, max=30)
    }
    else if (input$no_clusters == 1) {
      value <- "no_cluster"
      updateSelectInput(session, "options", selected = value,
                        choices = c("Choose one" = "",
                                    "no cluster",
                                    "ocean_proximity", 
                                    "longitude & latitude (using KMeans)"))
    }
  })
  
  observeEvent(input$no_clusters, {
    if (input$options == "no cluster") {
      value <- 1
      updateSliderInput(session, "no_clusters", value = value,
                        min = 1, max = 30)
    }
    else if (input$no_clusters == 1) {
      value <- "no_cluster"
      updateSelectInput(session, "options", selected = value,
                  choices = c("Choose one" = "",
                              "no cluster",
                              "ocean_proximity",
                              "longitude & latitude (using KMeans)"))
    }
  })
  
  # Fix clusters number is 4 in case Clustering by "ocean_proximity" is selected. ----
  observeEvent(input$options, {
    if (input$options == "ocean_proximity") {
      value <- 4
      updateSliderInput(session, "no_clusters", value = value,
                         min = 1, max=30)
    }
  })
  
  observeEvent(input$no_clusters, {
    if (input$options == "ocean_proximity") {
      value <- 4
      updateSliderInput(session, "no_clusters", value = value,
                         min = 1, max = 30)
    }
  })
  
  # Activate training process ----
  observeEvent(input$train, {
    if (input$options == "") {
      output$message <- renderPrint({
        cat("<WARNING> You must select options in \"Cluster by\" section to train the model!")
      })
    }
    else {
      updateTextInput(session, "progress", "Progress:", value = "Running")
      output$message <- renderPrint({
        cat("Our model is running ...")
      })
    }
  })
  
  # Model running process ----
  observeEvent(input$progress, {
    
    if (input$progress == "No need to input") {
      output$message <- renderPrint({
        avg_mape <- avg.MAPE(dir = dir)
        cat("Please complete the input to train the model!")
        cat(paste0("\nThe current model's MAPE is ", round(avg_mape*100, 2), "%"))
      })
    }
    
    if (input$progress == "Running") {
      
      # Train model ----
      train_model(cluster_by = input$options, no_clusters = input$no_clusters, 
                  dir = dir)
      
      # Save test set's MAPE results ----
      # function from "./utils/test-model.R"
      evaluate(dir = dir)
      avg_mape <- avg.MAPE(dir = dir)
      
      # Display test set's MAPE results ----
      output$accuracy <- renderPlot({
        # function from "./utils/visualization.R"
        vizMAPEs(dir = dir)
      })
      
      # Display house price prediction ----
      output$result <- renderPrint({
        long <- extractLong()
        lat <- extractLat()
        # function from "./utils/prediction.R"
        do.Predict(long, lat, dir = dir)
      })
      
      # Display location map ----
      output$plot <- renderPlot({
        long <- extractLong()
        lat <- extractLat()
        # function from "./utils/visualization.R"
        vizOnMap(long = long, lat = lat, dir = dir)
      })
      
      # Display correlation between independent and
      # dependent variables based on related cluster ----
      output$correlation <- renderTable({
        long <- extractLong()
        lat <- extractLat()
        # function from "./utils/correlation.R"
        pearson_corr(long, lat, dir = dir)
      })
      
      # Display progress status after training process finishes ----
      output$message <- renderPrint({
        cat("The training process has been done!")
        cat(paste0("\nThe current model's MAPE is ", round(avg_mape*100, 2), "%"))
      })
      
      updateTextInput(session, "progress", "Progress:", value = "Done")
    }
  })
  
  # Display test set's MAPE results ----
  output$accuracy <- renderPlot({
    # function from "./utils/correlation.R"
    vizMAPEs(dir = dir)
  })
  
  # Display location map ----
  output$plot <- renderPlot({
    long <- extractLong()
    lat <- extractLat()
    # function from "./utils/visualization.R"
    vizOnMap(long = long, lat = lat, dir = dir)
  })
  
  # Display house price prediction ----
  output$result <- renderPrint({
    long <- extractLong()
    lat <- extractLat()
    # function from "./utils/prediction.R"
    do.Predict(long, lat, dir = dir)
  })
  
  # Display correlation between independent and
  # dependent variables based on related cluster ----
  output$correlation <- renderTable({
    long <- extractLong()
    lat <- extractLat()
    # function from "./utils/correlation.R"
    pearson_corr(long, lat, dir = dir)
  })
  
  # Display information summary for looked-up location ----
  output$summary <- renderTable({
    long <- extractLong()
    lat <- extractLat()
    # function from "./utils/summary.R"
    detailed_info(long, lat, dir = dir)
  })
  
  # Display 100 random pairs of longitude and latitude to help users try the app ----
  output$try <- renderDataTable({
    # function from "./utils/try-examples.R"
    datatable(try_examples(long, lat, size = 100, dir = dir),
              options = list(paging = TRUE), 
              rownames = FALSE,
              class = list(stripe = FALSE))
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)


