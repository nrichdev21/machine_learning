library(ISLR)
library(class)
library(ggplot2)
library(caTools)

str(iris)
table(iris$Species)

# Standardize all but Species type - Note the information leakage into test set
standardized.features <- scale(iris[1:4])

# Combine back in the species type
full.df <- cbind(standardized.features, iris["Species"])

# Split the data
sample <- sample.split(full.df$Species, SplitRatio = .70)
train <- subset(full.df, sample == TRUE)
test <- subset(full.df, sample == FALSE)

# Train the model
pred.species <- NULL
error.rate <- NULL

for (i in 1:10){
  pred.species <- knn(train[1:4], test[1:4], train$Species, k=i)
  error.rate[i] <- mean(test$Species != pred.species)
}

# Plot the error rate
k.values <- 1:10
error.rate.df <- data.frame(k.values, error.rate)

ggplot(error.rate.df, aes(x=k.values, y= error.rate)) + geom_point() + geom_line()

