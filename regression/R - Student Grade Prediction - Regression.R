library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)

df <- read.csv('student-mat.csv', sep = ';', header=T)

# Check for null values
any(is.na(df))

#Check categorical variables, make sure they are factors
str(df)
df <- df %>% mutate_if(is.character, as.factor)

# Examine grades throughout semseter.  Final Grade (G3 is the response variable)
grades <- select(df, G1, G2, G3)
grades.stacked <- stack(grades)
grades.stacked.bp <- ggplot(grades.stacked, aes(ind, values)) + geom_boxplot()
grades.stacked.bp

grades.hg <- ggplot(gather(grades), aes(value)) +  geom_histogram(bins = 20) + 
            facet_wrap(~key)
grades.hg

for(i in 1:ncol(grades)){
ggplot(df, aes(x=grades[,i])) + geom_histogram(bins=20, fill='blue')
}

# Examine Correlation between the features
num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[num.cols])
corrplot(cor.data, method = 'color')

corrgram.plt <- corrgram(cor.data, order=T, lower.panel = panel.shade,
                         upper.panel=panel.pie, text.panel=panel.txt)

# Split data
sample <- sample.split(df$G3 , SplitRatio = 0.7)
train <- subset(df, sample ==T)
test <- subset(df, sample ==F)

# Train Model
model <- lm(G3 ~ ., train)
summary(model)             

# Examine residuals - Prefer Normal Distribution - Means distance from 
# actual values to predicted values are small
res <- residuals(model)
res <- as.data.frame(res)
ggplot(res, aes(res)) + geom_histogram(fill='blue')

# Examine plots
plot(model)

# Make predictions
G3.predictions <- predict(model, test)
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted','actuals')
results <- as.data.frame(results)

# Deal with negative prediction values
to_zero <- function(x){
  if(x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted, to_zero)

# Examine performance
MSE <- round(mean((results$actuals - results$predicted)^2),4)
RMSE <- round(sqrt(MSE),4)
print(paste("RMSE: ", RMSE))

SSE <- sum((results$predicted - results$actuals)^2)
SST <- sum((mean(df$G3) - results$actuals)^2)
R2 <- 1 - SSE/SST
print(paste("R2: ", R2))

# Calculate Adjusted R^2
1 - ((1-R2^2) * (nrow(results) - 1)) / (nrow(results - 1 - ncol(results)))
