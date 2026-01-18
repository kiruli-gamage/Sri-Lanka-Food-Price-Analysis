#Load the dataset 
library(tidyverse)
library(forcats)

# Step 2: Load the data
getwd()
data=read.csv("C:/Users/USER/Downloads/Food prices.csv")

# Convert usdprice to numeric
data$usdprice <- as.numeric(data$usdprice)

# Remove rows with missing values in usdprice
data <- na.omit(data)

# Create binary target variable: 1 if above median, else 0
median_price <- median(data$usdprice, na.rm = TRUE)
data$expensive <- ifelse(data$usdprice > median_price, 1, 0)

# Group rare factor levels into "Other"
data$category <- fct_lump(factor(data$category), n = 5)     # Top 5 categories
data$commodity <- fct_lump(factor(data$commodity), n = 10)  # Top 10 commodities

# Fit logistic regression model with increased iterations
model <- glm(expensive ~ category + commodity,
             data = data,
             family = binomial,
             control = glm.control(maxit = 100))

# View model summary
summary(model)

# Predict and evaluate
data$predicted_prob <- predict(model, type = "response")
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Confusion matrix
table(Actual = data$expensive, Predicted = data$predicted_class)