# Step 1: Install and load required packages

library(arules)

# Step 2: Load the data
getwd()
data=read.csv("C:/Users/USER/Downloads/Food prices.csv")

#Step 3:View the structure of the dataset
str(data)
head(data)

# Step 4: Preprocess (Assuming you want to find associations between food items purchased together)

transactions = read.transactions("C:/Users/USER/Downloads/Food prices.csv", format = "basket", sep = ",")


# Step 5: Generate rules
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.5))

# Step 6: Inspect rules
inspect(head(rules, by = "lift"))

