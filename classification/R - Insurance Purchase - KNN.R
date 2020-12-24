library(ISLR)
library(class)
library(ggplot2)

# Examine label
summary(Caravan$Purchase)

# Check for missing values
any(is.na(Caravan))

# Scaling is required.
var(Caravan[,1])
var(Caravan[,2])

purchase <- Caravan[,'Purchase']

# Standardize all but Purchase
standardized.Caravan <- scale(Caravan[, names(Caravan) != "Purchase"])

var(standardized.Caravan[,1])
var(standardized.Caravan[,2])

# Create test and train set
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

# Fit Model and make predictions
predicted.purchase <- knn(train.data, test.data, train.purchase, k = 3)
head(predicted.purchase)

misclass.error <- mean(test.purchase != predicted.purchase)
misclass.error

# Choosing a K value
predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

error.rate

#Plot error rate
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)

ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(
  lty='dotted', color='red')

