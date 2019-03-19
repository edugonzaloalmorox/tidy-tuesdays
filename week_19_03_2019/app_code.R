##########################
# Tidytuesday 
# Week: 19/02/2019
# @ EdudinGonzalo
##########################

# Code for the app "The Stanford Open Policing Project meets #tidytuesday"



library(shiny)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(shinythemes)


combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

data_app = combined_data %>%
  select(location:search_rate, arrest_rate) %>%
  filter(stop_rate <=0.4) %>%
  rename(Stop.Rate = stop_rate,
         Search.Rate = search_rate,
         Arrest.Rate = arrest_rate,
         Driver.Race = driver_race, 
         State = state)



# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-variable",
                  choices = c("Stop rate"      = "Stop.Rate", 
                              "Search rate"  = "Search.Rate", 
                              "Arrest rate"        = "Arrest.Rate"), 
                  selected = "Stop.Rate"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-variable:",
                  choices = c("Stop rate"      = "Stop.Rate", 
                              "Search rate"  = "Search.Rate", 
                              "Arrest rate"        = "Arrest.Rate"), 
                  selected = "Arrest.Rate"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("State" = "State", 
                              "Driver race" = "Driver.Race"),
                  selected = "state")
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = data_app, aes_string(x = input$x, y = input$y,
                                       color = input$z)) +
      geom_point() + 
      theme_minimal() +
      labs(title = "The Stanford Open Policing Project meets #tidytuesday", caption = "Source: Stanford Open Policing Project | @EdudinGonzalo ") +
      font("title", size = 17, color = "#1c81af", face = "bold")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)