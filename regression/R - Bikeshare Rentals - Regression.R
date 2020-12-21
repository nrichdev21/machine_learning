library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)
library(randomForest)
library(rpart)
library(Ecdat)

df <- read.csv('bikeshare.csv', header= T)

# Check for missing values
any(is.na(df))

# Check unique values in each feature and the label
rapply(df, function(x) length(unique(x)))


# Function to convert numeric to factor columns to see if it has an affect on the model
df$season <- factor(df$season)
    # 1 = spring, 2 = summer, 3 = fall, 4 = winter 
df$holiday <- factor(df$holiday)
df$workingday <- factor(df$workingday)
df$weather <- factor(df$weather)
  # 1. Clear and Sunny - 4. Thunderstorm


# Convert datetime feature into proper format
df$datetime <- as.POSIXct(df$datetime, format="%Y-%m-%d %H:%M:%S")
df$day <- format(df$datetime, "%Y-%m-%d")
df$time <- sapply(df$datetime, function(x){format(x,"%H")}) %>%
  sapply(as.numeric)

# Check correlations
num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[num.cols])
corrplot(cor.data, method = 'color')
count.corr <- as.data.frame(cor.data[,'count'])

# Check registered and casual rentals relationship with count
any(df$casual + df$registered == df$count) != T
# Remove these features from the model

# Effect of Temperature on rentals
ggplot(df, aes(temp, count)) + geom_jitter(alpha=0.05, color= 'blue')

# Did the rideshare program gain popularity? 
ggplot(df, aes(day, count)) + geom_jitter(aes(color=temp), alpha=0.1) + 
  scale_color_gradientn(colors= c('dark blue','blue','light blue','light green',
                                  'yellow','orange','red'))

# Popular Rental Times
ggplot(df, aes(time, count)) + geom_jitter(aes(color=temp), alpha=0.3) + 
  scale_color_gradientn(colors= c('dark blue','blue','light blue','light green',
                                   'yellow','orange','red'))

# Seasonality
ggplot(df, aes(season, count)) + geom_boxplot(aes(color=factor(season)))

# Work days vs non workdays
ggplot(filter(df, workingday == 1), aes(time, count)) + geom_jitter(aes(color=temp), alpha=0.3) + 
  scale_color_gradientn(colors= c('dark blue','blue','light blue','light green',
                                  'yellow','orange','red'))

ggplot(filter(df, workingday == 0), aes(time, count)) + geom_jitter(aes(color=temp), alpha=0.3) + 
  scale_color_gradientn(colors= c('dark blue','blue','light blue','light green',
                                  'yellow','orange','red'))

# Remove features that are not useful or give duplicate information
df.clean <- subset(df, select= -c(registered, casual, datetime, day, atemp))

# Split data
sample <- sample.split(df.clean$count , SplitRatio = 0.7)
train <- subset(df.clean, sample ==T)
test <- subset(df.clean, sample ==F)

# Train the model
model <- lm(count ~ ., train)
summary(model)

# Count is not linear, try random forest regressor
model <- rpart(count ~ ., method='anova', data=train, )
summary(model)

# Create a simpler model based just upon temp
model <- lm(count ~ temp, train)
summary(model)
