library(rpart)
library(rpart.plot)
library(randomForest)

str(kyphosis)

# Decision Tree
tree <- rpart(Kyphosis ~ ., method='class', data = kyphosis)

printcp(tree)

# One Way - not as nice
plot(tree, uniform = T, main= 'Kyphosis Tree')
text(tree, use.n= T, all=T)

# Using rpart.plot library
prp(tree)

rf.model <- randomForest(Kyphosis ~ ., data=kyphosis)
print(rf.model)

rf.model$predicted
rf.model$ntree
rf.model$err.rate
rf.model$proximity
