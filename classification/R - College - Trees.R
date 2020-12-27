library(caTools)
library(rpart.plot)
library(rpart)
library(Amelia)
library(randomForest)
library(ISLR)
library(ggplot2)
library(corrplot)
library(dplyr)

# Load Public/Private School Dataset
df <- College

head(df)
str(df)

# Examine Label Distribution
table(df$Private)

# Check for missing values
missmap(df, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Examine Correlations
num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[num.cols])
corrplot(cor.data, method = 'color')

# Graduate Rate vs Expenditures
ggplot(df, aes(Grad.Rate, Expend)) + geom_point(aes(color=factor(Private))) +
  scale_x_continuous(breaks= seq(min(0), max(100), by=10))

# Remove any records where grad rate > 100% 
df <- filter(df, Grad.Rate <= 100)

ggplot(df, aes(Grad.Rate, Expend)) + geom_point(aes(color=factor(Private))) +
  scale_x_continuous(breaks= seq(min(0), max(100), by=10))

# Graduation Rate and number of undergrad (Full and Part Time)
df$total.students <- df$F.Undergrad + df$P.Undergrad

ggplot(df, aes(Grad.Rate, total.students)) + 
  geom_point(aes(color=factor(Private))) + 
  scale_x_continuous(breaks= seq(min(0), max(100), by=10))
  
# Closer Look at graduation rate
ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill=factor(Private)), 
                                            color='black')

# Split data
sample <- sample.split(df$Private, 0.7)
train <- subset(df, sample==T)
test <- subset(df, sample==F)

# Decision Tree
d.tree <- rpart(Private ~ ., method='class', data = train)
printcp(d.tree)

prp(d.tree)

# Make Predictions
pred.tree <- predict(d.tree, test)
pred.tree <- as.data.frame(pred.tree)

# Function that will assign public or private based upon predicted probabilities
assign_class <- function(x){
  if (x>=0.5){
    return(1)
  }else{
    return(0)
  }
}

pred.tree$Private <- sapply(pred.tree$Yes, assign_class)

# Confusion Matrix
table(pred.tree$Private, test$Private)

# Build Random Forest Model
rf.model <- randomForest(Private ~ ., data=train, importance=T)
print(rf.model)
rf.model$importance

#Make Predictions
pred.rf <- predict(rf.model, test)

#Confusion Matrix
table(pred.rf, test$Private)
