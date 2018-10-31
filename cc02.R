library(tidyverse)
library(lubridate)
#install.packages("caret")
library(caret)
#install.packages("dslabs")
library(dslabs)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
y

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

cutoff <- seq(4, 7, by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

str(y_hat)
str(train$Species)

plot(cutoff, accuracy)

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#now let's try on test
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") #%>% 
  #factor(levels = levels(test$Species))

str(y_hat)
str(test$Species)

# To avoid the error : Error in Ops.factor(y_hat, test$Species) : 
# level sets of factors are different

y_hat <- factor(y_hat, levels=c("setosa","versicolor","virginica"))

str(y_hat)
str(test$Species)

mean(y_hat == test$Species)


# Working the the test dataframe, we notice that the best indicator is now the Patal.witdh
View(test)

# Optimize the combination of the cutoffs for Petal.Length and Petal.Width in the train data and report the overall a
# ccuracy when applied to the test dataset.

# For simplicity, create a rule that if either the length OR the width is greater than the length cutoff or the width cutoff 
# then virginica or versicolor is called.

cutoff_L <- seq(4, 7, by=0.1)
cutoff_W <- seq(0, 3, by=0.1)

accuracy <- map_dbl(cutoff_L, function(x){
  y_hat <- ifelse(train$Petal.Length > x , "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

plot(cutoff_L, accuracy)

max(accuracy)
best_cutoff_L <- cutoff_L[which.max(accuracy)]
best_cutoff_L


accuracy <- map_dbl(cutoff_W, function(x){
  y_hat <- ifelse(train$Petal.Width > x , "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

plot(cutoff_W, accuracy)

max(accuracy)
best_cutoff_W <- cutoff_W[which.max(accuracy)]
best_cutoff_W

#now let's try on test
y_hat <- ifelse(test$Petal.Length > best_cutoff_L & test$Petal.Width > best_cutoff_W, "virginica", "versicolor") #%>% 
#factor(levels = levels(test$Species))

# To avoid the error : Error in Ops.factor(y_hat, test$Species) : 
# level sets of factors are different
y_hat <- factor(y_hat, levels=c("setosa","versicolor","virginica"))

mean(y_hat == test$Species)

