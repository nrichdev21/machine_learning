library(Amelia)
library(ggplot2)
library(dplyr)
library(plyr)
library(caTools)
library(tidyr)

titanic.train <- read.csv('titanic_train.csv', stringsAsFactors = T)
titanic.test <- read.csv('titanic_test.csv', stringsAsFactors = T)

# Check for missing values
missmap(titanic.train, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Passengers Survived
ggplot(titanic.train, aes(Survived)) + geom_bar()

# Fare Classes
ggplot(titanic.train, (aes(Pclass))) + geom_bar(aes(fill=factor(Pclass)))

# Male vs Female
ggplot(titanic.train, (aes(Sex))) + geom_bar(aes(fill=factor(Sex)))

# Age distribution
ggplot(titanic.train, aes(Age)) + geom_histogram(bins=20, alpha=0.5, 
                                                 fill= 'dark blue',
                                                 color='black')

# Siblings and Spouses
ggplot(titanic.train, aes(SibSp)) + geom_bar()

# Fares paid
ggplot(titanic.train, aes(Fare)) + geom_histogram(fill=' light green',
                                                  color='black')

# Impute missing values for age using Class mean.
pl <- ggplot(titanic.train, aes(Pclass, Age))
pl <- pl + geom_boxplot(aes(group=Pclass, fill= factor(Pclass)))
pl <- pl + scale_y_continuous(breaks= seq(min(0), max(80), by=2))
pl + theme_bw()

impute_age <- function(age, class){
  out <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        out[i] <- 37
      }else if (class[i] == 2){
        out[i] <- 29
      }else {
        out[i] <- 24
      }
    }else{
      out[i] <- age[i]
    }
  }
  return(out)
}

ages <- impute_age(titanic.train$Age, titanic.train$Pclass)
titanic.train$Age <- ages
missmap(titanic.train, main = 'Imputation Check', col = c('yellow','black'),
        legend=F)

# Remove features that are not useful
passengerID, Name, Ticket, Cabin
titanic.train <- select(titanic.train, -PassengerId, -Name, -Ticket, -Cabin)

# Convert features from continuous to factor data type
titanic.train$Survived <- factor(titanic.train$Survived)
titanic.train$Pclass <- factor(titanic.train$Pclass)
titanic.train$Parch <- factor(titanic.train$Parch)
titanic.train$SibSp <- factor(titanic.train$SibSp)

# Train model
log.model <- glm(Survived ~ . , family = binomial(link = 'logit'),
                 data = titanic.train)
summary(log.model)

# Train test using just training set
split <- sample.split(titanic.train$Survived, SplitRatio = 0.7)
final.train <- subset(titanic.train, split == T)
final.test <- subset(titanic.train, split == F)

final.log.model <- glm(Survived ~ . , family = binomial(link = 'logit'),
                       data = final.train)
summary(final.log.model)

# Make predictions

# Numbers of levels for Parch could be different between train and test set.  
# This is a workaround:
# levels(final.train$Parch)
# levels(final.test$Parch)
# final.test$Parch <-  revalue(final.test$Parch,c("9" = "6"))

fitted.probabilities <- predict(final.log.model, newdata=final.test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)
misClasificError <- mean(fitted.results != final.test$Survived)
print(paste('Accuracy', 1- misClasificError))

# Confusion matrix
table(final.test$Survived, fitted.probabilities > 0.5)

