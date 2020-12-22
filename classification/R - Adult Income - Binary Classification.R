library(Amelia)
library(ggplot2)
library(dplyr)
library(plyr)
library(caTools)
library(tidyr)

adult <- read.csv('adult_sal.csv', stringsAsFactors = T)

adult[adult == '?'] <- NA

# Check for missing values
missmap(adult, main = 'Missing Map', col = c('yellow','black'),
        legend=F)

colSums(is.na(adult))

# Remove N/A's since they represent small percentage of data.  Data missing
# from both occupation and type_employer features indicate further suspicsion
# on these records.
adult <- na.omit(adult)
missmap(adult, main = 'Missing Map', col = c('yellow','black'),
        legend=F)
# Remove duplicate column
adult <- select(adult, -X)

# Unique values for each feature
rapply(adult, function(x)length(unique(x)))

# Unbalanced label
summary(adult$income)

# Same information is available in both of these columns.  Keep education_num
unique(select(adult, education, education_num))
adult <- select(adult,-education)

# Combined employer type categories into smaller groups
# Keep the values codes as '?' in case missing employment data is significant
# or could just be missing day in which remove.
table(adult$type_employer)

unemployed <- function(job){
  job <- as.character(job)
  if (job == 'Never-worked' | job == "Without-pay"){
    return("Unemployed")
  }else if (job == 'Local-gov' | job == 'State-gov'){
    return('SL-gov')
  }else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self-Emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemployed)
table(adult$type_employer)

# Combine marital status categories
table(adult$marital)
married <- function(st){
  st <- as.character(st)
  if (st == 'Divorced' | st == 'Separated' | st == 'Widowed'){
    return('Not Married')
  }else if (st == 'Married-AF-spouse' | st == 'Married-civ-spouse' |
            st == 'Married-spouse-absent'){
    return('Married')
  }else{
    return(st)
  }
}

adult$marital <- sapply(adult$marital, married)  
table(adult$marital)

# Group countries by geographic areas
table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('Other')

country_grouping <- function(cntry){
  if (cntry %in% Asia){
    return('Asia')
  }else if (cntry %in% North.America){
    return('North America')
  }else if (cntry %in% Europe){
    return('Europe')
  }else if (cntry %in% South.America){
    return('South America')
  }else{
    return('Other')
  }
}

adult$country <- sapply(adult$country, country_grouping)
table(adult$country)

# Convert modified features back into factor data types
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)


# Age vs. Income
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',
                                        binwidth=1) + theme_bw()

# Hours per week vs. Income
ggplot(adult, aes(hr_per_week)) + geom_histogram(bins=40, aes(fill=income)) +
  scale_x_continuous(breaks= seq(min(0), max(100), by=5))

# Country vs. Hours worked
ggplot(adult, aes(hr_per_week)) + geom_histogram(bins=40, aes(fill=country)) +
  scale_x_continuous(breaks= seq(min(0), max(100), by=5))

# Ex-USA vs. Hours Worked
ggplot(filter(adult, country != 'North America') , aes(hr_per_week)) + 
  geom_histogram(bins=40, aes(fill=country)) +
  scale_x_continuous(breaks= seq(min(0), max(100), by=5))

# Income vs. Region
ggplot(adult, aes(country)) + geom_bar(aes(fill=income)) + theme_grey()

# Income by Occupation in the US
ggplot(filter(adult, country == 'North America'), aes(occupation)) +
  geom_bar(aes(fill=income)) + theme(axis.text.x = element_text(angle=90))

# Train Model
split <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, split == T)
test <- subset(adult, split == F)

model <- glm(income ~ . , family = binomial(link = 'logit'),
                       data = train)
summary(model)

step.model = step(log.model)
summary(step.model)

# Examine Performance
test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)

# Accuracy
(6287 + 1384) / (6287 + 1384 + 868 + 509)

#Recall
(6287) / (6287 + 509)

#Precision
(6287) / (6287 + 868)
