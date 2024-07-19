#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Install necessary packages
if (!require('shiny')) install.packages('shiny')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('dplyr')) install.packages('dplyr')

library(shiny)
library(ggplot2)
library(dplyr)

# Define the file path
file_path <- "~/netflix_titles.csv"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("File not found! Please check the file path.")
}

# Load the data
netflix_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Convert duration to numeric for movies
netflix_data <- netflix_data %>%
  mutate(duration_numeric = ifelse(grepl('min', duration), as.numeric(gsub(' min', '', duration)), NA))

# Define UI
ui <- fluidPage(
  titlePanel("Netflix Titles Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose Plot Type:", 
                  choices = c("Bar Chart", "Histogram", "Boxplot"))
    ),
    
    mainPanel(
      plotOutput("netflixPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  output$netflixPlot <- renderPlot({
    if (input$plotType == "Bar Chart") {
      ggplot(netflix_data, aes(x = rating)) +
        geom_bar() +
        theme_minimal() +
        labs(title = "Count of Netflix Titles by Rating", x = "Rating", y = "Count")
      
    } else if (input$plotType == "Histogram") {
      ggplot(netflix_data, aes(x = release_year)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "white") +
        theme_minimal() +
        labs(title = "Distribution of Release Years", x = "Release Year", y = "Count")
      
    } else if (input$plotType == "Boxplot") {
      ggplot(netflix_data, aes(x = type, y = duration_numeric)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Duration of Movies and TV Shows", x = "Type", y = "Duration (min)")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
