library(tidyverse)
library(caret)

#The RMSE of the train_set (rmse of fit$residuals)
#=================================================
set.seed(1) 
results <- function(){
  
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x , data = train_set)
  r_mse <- sqrt(mean(fit$residuals^2))
}

R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)

> R_MSE <- replicate(n, results())
> mean(R_MSE)
[1] 2.401705
> sd(R_MSE)
[1] 0.1306818

#The RMSE of the the test_set (y_hat vs y)
#===========================================
set.seed(1)

n <- 100

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
results <- function(){
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

> mean(R_MSE)
[1] 2.488661
> sd(R_MSE)
[1] 0.1243952


R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)



#N <- c(100, 500, 1000, 5000, 10000)
N <- c(10, 20, 30, 40, 50)

results <- function(){
  #  dat <- map(N, dat)
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

set.seed(1)
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
 data.frame() %>% setNames(c("x", "y"))

set.seed(1)
R_MSE <- replicate(100, results())
mean(R_MSE)
sd(R_MSE)


set.seed(1)
myRMSE <- function (size){    
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSEs <- replicate(100, {
                     y <- dat$y
                     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
                     train_set <- dat %>% slice(-test_index)
                     test_set <- dat %>% slice(test_index)
                     fit <- lm(y ~ x , data = train_set)
                     y_hat <- predict(fit, test_set)
                     rmse <- sqrt(mean((y_hat - test_set$y)^2))
                    } )
  list( mean(RMSEs), sd(RMSEs) )
}

size <- c(100, 500, 1000, 5000, 10000)

set.seed(1)
RES <- map(size, myRMSE)
RES



set.seed(1)
myRMSE <- function (size){    
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSEs <- replicate(100, {
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
  } )
  list( mean(RMSEs), sd(RMSEs) )
}

set.seed(1)
myRMSE(100)



set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
results <- function(){
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}


set.seed(1)
R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)



set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1)
results <- function(){
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1 , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}


set.seed(1)
R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)


set.seed(1)
results <- function(){
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_2 , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

set.seed(1)
R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)


set.seed(1)
results <- function(){
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1 + x_2 , data = train_set)
  y_hat <- predict(fit, test_set)
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
}

set.seed(1)
R_MSE <- replicate(n, results())
mean(R_MSE)
sd(R_MSE)



set.seed(1)
myRMSE <- function (size){    
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 1000, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  RMSEs <- replicate(size, {
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_1 + x_2, data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
  } )
  list( mean(RMSEs), sd(RMSEs) )
}

set.seed(1)
myRMSE(100)




set.seed(1)
myRMSE <- function (size){    
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  RMSEs <- replicate(100, {
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_1 + x_2 , data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
  } )
  list( mean(RMSEs), sd(RMSEs) )
}

set.seed(1)
myRMSE(100)


set.seed(1)
myRMSE <- function (size){    
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  RMSEs <- replicate(100, {
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x_1 + x_2, data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
  } )
  list( mean(RMSEs), sd(RMSEs) )
}

set.seed(1)
myRMSE(100)


set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

str(dat)

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#ERROR
#Error during wrapup: `data` and `reference` should be factors with the same levels.
set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = delta, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
  
  glm_fit <- dat$train %>%
    glm(y ~ x, data=., family = "binomial" )
  
  p_hat <- predict(glm_fit, newdata = dat$test, type = "response")
  
  y_hat <- ifelse(p_hat > 0.5, 1, 0) #%>% factor
  #confusionMatrix(y_hat, dat$test$y)
  
}
#dat <- make_data()

delta <- seq(0, 3, len=25)

set.seed(2)
RES <- map(delta, make_data())


