library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)
library(randomForest)

bn <- read.csv('bank_note_data.csv')

str(bn)

# Check for missing values
missmap(bn, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Correlation between features
# Examine Correlations
num.cols <- sapply(bn, is.numeric)
cor.data <- cor(bn[num.cols])
corrplot(cor.data, method = 'color')

# Get unique counts for each column
apply(bn, 2, function(x) length(unique(x))) # 2 means apply over columns

# Split dataset for training
spt <- sample.split(bn, SplitRatio = 0.7)
train <- subset(bn, spt == T)
test <- subset(bn, spt == F)

# Parse feature names into formulas
n <- names(train)  # Feature Names
f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

# Build model
nn <- neuralnet(f, data=train, hidden = c(10,3), linear.output = F) 
plot(nn)

# Make Predictions and Evaluate
pred.class <- compute(nn, test[1:4])
pred <- sapply(pred.class$net.result, round)

table(pred, test$Class)

# Random Forest Model
bn$Class <- factor(bn$Class)
spt <- sample.split(bn, SplitRatio = 0.7)
train <- subset(bn, spt == T)
test <- subset(bn, spt == F)

model <- randomForest(f,data=train)

pred.rf <- predict(model, test)
table(pred.rf, test$Class)
