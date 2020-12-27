library(Amelia)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(e1071)

loan <- read.csv('loan_data.csv')

# Check for missing values
missmap(loan, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Correlation between features
# Examine Correlations
num.cols <- sapply(loan, is.numeric)
cor.data <- cor(loan[num.cols])
corrplot(cor.data, method = 'color')

# Get unique counts for each column
apply(loan, 2, function(x) length(unique(x)))

# Plot distribution of each variable
df.histogram <- loan %>% gather()

pl1 <- ggplot(df.histogram, aes(value)) + geom_bar()
pl2 <- pl1 + facet_wrap(~key,  scales = 'free')
pl2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank() )

# Convert categorical columns to factors

loan$credit.policy <- factor(loan$credit.policy)
loan$inq.last.6mths <- factor(loan$inq.last.6mths)
loan$delinq.2yrs <- factor(loan$delinq.2yrs)
loan$pub.rec <- factor(loan$pub.rec)
loan$not.fully.paid <- factor(loan$not.fully.paid)

# Loans Paid back in full is the response variable - Unbalanced Data Set
ggplot(loan, aes(not.fully.paid)) + geom_bar()

# Credit Scores
ggplot(loan, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), bins=30,
                                           color='black')

# Purpose of the loan
ggplot(loan, aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid),
                                              color='black', position = "dodge") +
  theme(axis.text.x = element_text(angle=90))

# FICO score and Interest Rate Received
ggplot(loan, aes(int.rate, fico)) + geom_jitter(aes(color=not.fully.paid), 
                                                alpha=0.4, size=1)

# Split the data
split <- sample.split(loan$not.fully.paid, 0.7)
train = subset(loan, split == T)
test = subset(loan, split == F)

# Grid Search and SVM
tune.results <- tune(svm, train.x = not.fully.paid ~ . , data = train, 
                     kernel ='radial', ranges= list(cost = c(1, 10),
                                                   gammas = c(0.1,1)))
summary(tune.results)


# Build the model with optimized hyperparameters
model <- svm(not.fully.paid ~ .,data=train ,cost=10,gamma = 0.1)
model

# Predict off the training set and evaluate performance.
pred.defaults <- predict(model, test[1:13])
table(pred.defaults, test$not.fully.paid)
