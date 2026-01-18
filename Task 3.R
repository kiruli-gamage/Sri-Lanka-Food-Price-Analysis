# Load required libraries
library(shiny)
library(tidyverse)
library(forcats)
library(plotly)
library(caret)
library(shinythemes)

# Load and clean data
data=read.csv("C:/Users/USER/Downloads/Food prices.csv")


# Clean numeric price column
data$usdprice <- as.numeric(gsub(",", "", data$usdprice))
data <- data %>% drop_na(usdprice, category, commodity)

# Create binary target
median_price <- median(data$usdprice)
data$expensive <- ifelse(data$usdprice > median_price, 1, 0)

# Convert and lump factor levels
data$category <- fct_lump(as.factor(data$category), n = 5)
data$commodity <- fct_lump(as.factor(data$commodity), n = 10)

# Fit logistic regression
model <- glm(expensive ~ category + commodity, data = data, family = binomial)
data$predicted_prob <- predict(model, type = "response")
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(data$predicted_class), as.factor(data$expensive))

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("??? Food Price Intelligence Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_category", "Choose Category:",
                  choices = c("All", levels(data$category)), selected = "All"),
      selectInput("selected_commodities", "Choose Commodities:",
                  choices = sort(unique(data$commodity)), selected = unique(data$commodity)[1:3],
                  multiple = TRUE),
      sliderInput("prob_range", "Filter by Predicted Probability:",
                  min = 0, max = 1, value = c(0, 1), step = 0.05),
      br(),
      checkboxInput("show_summary", "Show Summary Stats", TRUE),
      checkboxInput("show_bar", "Show Bar Chart (Actual vs Predicted)", FALSE)
    ),
    
    mainPanel(
      conditionalPanel("input.show_summary == true",
                       h4("Summary"),
                       verbatimTextOutput("summaryStats")
      ),
      conditionalPanel("input.show_bar == true",
                       h4("Actual vs Predicted Comparison"),
                       plotlyOutput("barPlot")
      ),
      h4("Predicted Probabilities by Commodity"),
      plotlyOutput("probPlot"),
      h4("Confusion Matrix"),
      verbatimTextOutput("confMatrix")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data
    if (input$selected_category != "All") {
      df <- df[df$category == input$selected_category, ]
    }
    df <- df[df$commodity %in% input$selected_commodities, ]
    df <- df[df$predicted_prob >= input$prob_range[1] & df$predicted_prob <= input$prob_range[2], ]
    df
  })
  
  output$probPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = commodity, y = predicted_prob, color = factor(expensive))) +
      geom_jitter(width = 0.2, height = 0) +
      labs(title = "Predicted Probabilities",
           x = "Commodity", y = "Probability", color = "Actual Expensive") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$confMatrix <- renderPrint({
    conf_matrix
  })
  
  output$summaryStats <- renderPrint({
    df <- filtered_data()
    summary_df <- df %>%
      summarise(
        Count = n(),
        Mean_Price = mean(usdprice),
        Mean_Prob = mean(predicted_prob),
        Expensive_Pct = mean(expensive) * 100
      )
    print(summary_df)
  })
  
  output$barPlot <- renderPlotly({
    df <- filtered_data()
    bar_data <- df %>%
      count(expensive, predicted_class) %>%
      mutate(expensive = ifelse(expensive == 1, "Actual Expensive", "Actual Cheap"),
             predicted_class = ifelse(predicted_class == 1, "Predicted Expensive", "Predicted Cheap"))
    
    p <- ggplot(bar_data, aes(x = expensive, y = n, fill = predicted_class)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Count", fill = "Prediction") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
