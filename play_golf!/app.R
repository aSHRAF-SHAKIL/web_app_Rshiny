#======================
#SHAKIL ASHRAF 
#DATE:1/19/2025 5:31AM


# Load libraries
library(shiny)
library(shinythemes)
library(randomForest)

# Import dataset
weather <- read.csv("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv")

# Convert target variable to factor
weather$play <- as.factor(weather$play)

# Convert predictor variables to factors (if necessary)
weather$outlook <- as.factor(weather$outlook)
weather$windy <- as.factor(weather$windy)

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Save model
saveRDS(model, "model.rds")

# Read model
model <- readRDS("model.rds")

# Define UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # Page header
  headerPanel('Should You Play Golf?'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input Parameters</h3>"),
    
    selectInput("outlook", label = "Outlook:", 
                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                selected = "Rainy"),
    sliderInput("temperature", "Temperature:",
                min = 64, max = 86,
                value = 70),
    sliderInput("humidity", "Humidity:",
                min = 65, max = 96,
                value = 90),
    selectInput("windy", label = "Windy:", 
                choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                selected = "TRUE"),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    # Create a data frame with user inputs
    df <- data.frame(
      outlook = input$outlook,
      temperature = input$temperature,
      humidity = input$humidity,
      windy = as.logical(input$windy)
    )
    
    # Ensure the input data has the same factor levels as the training data
    df$outlook <- factor(df$outlook, levels = levels(weather$outlook))
    df$windy <- factor(df$windy, levels = levels(weather$windy))
    
    # Make predictions
    prediction <- predict(model, df, type = "response")
    probabilities <- predict(model, df, type = "prob")
    
    # Combine results into a data frame
    Output <- data.frame(
      Prediction = prediction,
      Probability_No = round(probabilities[, 1], 3),
      Probability_Yes = round(probabilities[, 2], 3)
    )
    
    return(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
