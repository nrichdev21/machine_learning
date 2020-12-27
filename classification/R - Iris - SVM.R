library(ISLR)
library(e1071)

# Create Model using IRIS dataset
model <- svm(Species ~ . , data=iris)
summary(model)

# Make predictions and evaluate model
pred.val <- predict(model, iris[1:4])
table(pred.val, iris[,5])

# Tune Model w/ Grid Search
tune.results <- tune(svm, train.x = iris[1:4], train.y=iris[,5], kernel='radial',
                     ranges = list(cost=c(0.4,0.5,0.7), gamma=c(0.1,0.5,0.7)))

summary(tune.results)
# Cost = 0.4 Gamma = 0.1

# Seperate step required to create the optimized model
tuned.svm <- svm(Species ~ ., data=iris, kernel='radial', cost=0.4, Gamma=0.1)
summary(tuned.svm)
