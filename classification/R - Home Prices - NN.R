library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)

str(Boston)

# Check for missing values
missmap(Boston, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Correlation between features
# Examine Correlations
num.cols <- sapply(Boston, is.numeric)
cor.data <- cor(Boston[num.cols])
corrplot(cor.data, method = 'color')

# Get unique counts for each column
apply(Boston, 2, function(x) length(unique(x))) # 2 means apply over columns

# Plot distribution of each variable
df.histogram <- Boston %>% gather()

pl1 <- ggplot(df.histogram, aes(value)) + geom_bar()
pl2 <- pl1 + facet_wrap(~key,  scales = 'free')
pl2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank() )

Bos <- Boston

# Normalize Data with min-max scaler
maxs <- apply(Bos, 2, max)
mins <- apply(Bos, 2, min)

scaled.df <- scale(Bos, center=mins, scale= maxs-mins)
scaled <- as.data.frame(scaled.df)

# Split data set for training and testing
split <- sample.split(scaled$medv, SplitRatio = 0.7)
train <- subset(scaled, split==T)
test <- subset(scaled, split==F)

# Build and Train Model
n <- names(train)  # Feature Names

# Parse into format neuralnet function expects
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f, data=train, hidden = c(5,3), linear.output = T) 
# T for regression, F for classifcation on linear.output

# Plot neural network
plot(nn)

# Predictions and Evaluate
pred.prices <- compute(nn, test[1:13])

# Undo scaling
pred.true <- pred.prices$net.result * (max(Bos$medv)-min(Bos$medv))+min(Bos$medv)
test.unscaled <- (test$medv)*(max(Bos$medv)-min(Bos$medv))+min(Bos$medv)

MSE.nn <- sum((test.unscaled - pred.true)^2/nrow(test))
MSE.nn

# Dataframe with actual vs predicted
error.df <- data.frame(test.unscaled, pred.true)
error.df

# Plot Actual vs. Predicted
ggplot(error.df, aes(test.unscaled, pred.true)) + geom_point() + stat_smooth()
