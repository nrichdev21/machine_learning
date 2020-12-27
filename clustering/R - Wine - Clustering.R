library(ggplot)
library(cluster)
library(ggplot2)
library(Amelia)
library(corrplot)
library(dplyr)
library(tidyr)

red.wine <- read.csv('winequality-red.csv', sep=';')
white.wine <- read.csv('winequality-white.csv',sep=';')

# Assign labels to evaluate against later.
red.wine$color <- 'Red'
white.wine$color <- 'White'

# Combine into one dataframe
wine <- rbind(red.wine, white.wine)

# Check for missing values
missmap(wine, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

# Correlation between features
# Examine Correlations
num.cols <- sapply(wine, is.numeric)
cor.data <- cor(wine[num.cols])
corrplot(cor.data, method = 'color')

# Get unique counts for each column
apply(wine, 2, function(x) length(unique(x)))

# Plot distribution of each variable
df.histogram <- wine %>% gather()

pl1 <- ggplot(df.histogram, aes(value)) + geom_bar()
pl2 <- pl1 + facet_wrap(~key,  scales = 'free')
pl2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank() )

# Sugar  between wine types.
ggplot(wine, aes(residual.sugar)) + geom_histogram(aes(fill=color), color='black',
                                                   bins=40) +
  scale_fill_manual(values=c('maroon','grey')) + theme_bw()

# Acid between wine types.
ggplot(wine, aes(citric.acid)) + geom_histogram(aes(fill=color), color='black',
                                                   bins=40) +
  scale_fill_manual(values=c('maroon','grey')) + theme_bw()

# Quality Distribution between wines.
ggplot(wine, aes(quality)) + geom_bar(aes(fill=color)) +
  scale_fill_manual(values=c('maroon','grey')) + theme_bw()


# Citris Acid vs Residual Sugars
ggplot(wine,aes(citric.acid,residual.sugar)) + 
  geom_point(aes(color=color),alpha=0.3) + 
  scale_color_manual(values=c('maroon','grey')) + theme_bw()

# Free and Total Sulfur Dioxide
ggplot(wine,aes(free.sulfur.dioxide, total.sulfur.dioxide)) + 
  geom_point(aes(color=color),alpha=0.3) + 
  scale_color_manual(values=c('maroon','grey')) + theme_bw()

# Remove label
df.clus <- wine[, !names(wine) %in% c("color")] 

# Build Model with multiple K values 
model.clus <- NULL
error.rate <- NULL

for (i in 1:5){
  model.clus <-  kmeans(df.clus, center= i, nstart=5)
  error.rate[i] <- 1 - (model.clus$betweenss / model.clus$totss) * 100
}

#Plot error rate
k.values <- 1:5
error.df <- data.frame(error.rate, k.values)

ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(
  lty='dotted', color='red')

# Choose model with 2 clusters - Technically cheating since we knew ahead of time.
model <- kmeans(df.clus, center= 2, nstart = 5)

# Compare to original data.
table(model$cluster, wine$color)
# CONCLUSION: Red is easier to cluster together 

# Plot with 2 principal components
clusplot(wine, model$cluster, color=T, shade=T, labels=0, lines=0)
                
