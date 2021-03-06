setwd("/users/mateuszzezula/Box/1. First Semester/Programming for Data Science/Homework/homework_13/Homework-13/Zezula")

library(pacman)
p_load(dplyr, caret)

if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header = F) %>% as.matrix
  classify <- mtrain[, 1]
  y <- factor(classify, levels = c(0, 1))
  
  for (i in 1:length(classify)) {
    number <- classify[i]
    if (number == 3){
      number <- 1
    } else {
      number <- 0
    }
    y[i] <- number
  }
  y <- factor(y, levels = c(0, 1))
  y <- y[1:1000]
  
  mtrain <- mtrain[,-1]/256 
  colnames(mtrain) <- 1:(28^2)
  x <- mtrain[1:1000,]
}

if (!exists("mtest")) {
  mtest <- read.csv("mnist_train.csv", header = F) %>% as.matrix
  classify <- mtest[, 1]
  test.y <- factor(classify, levels = c(0, 1))
  
  for (i in 1:length(classify)) {
    number <- classify[i]
    if (number == 3){
      number <- 1
    } else {
      number <- 0
    }
    test.y[i] <- number
  }
  test.y <- factor(test.y, levels = c(0, 1))
  test.y <- test.y[1001:2000]
  
  mtest <- mtest[,-1]/256 
  colnames(mtest) <- 1:(28^2)
  test.x <- mtest[1001:2000,]
}

prediction_error <- function(x, y, nnet.result) {
  true_y <- y
    pred_y <- predict(nnet.result, x)
  
  n_samples <- nrow(x)
  error <- sum(true_y != pred_y)/n_samples
  return (error)
}

########################################

# Training with decay = 0 

tuning_df.nd <- data.frame(size = 5:10, decay = 0)
fitControl.nd <- trainControl(method = "none")
fitControl.nd <- trainControl(method = "repeatedcv",
                              number = 2,
                              repeats = 3)

out.nd <- caret::train(x = x, y = y, method = "nnet",
                      trControl = fitControl.nd,
                      tuneGrid = tuning_df.nd, 
                      maxit = 1000, 
                      MaxNWts = 10000)

pred_error <- prediction_error(x, y, out.nd)
cat("training dataset prediction error", pred_error, "\n")

pred_error <- prediction_error(test.x, test.y, out.nd)
cat("test dataset prediction error", pred_error, "\n")

# Training with varying decay

tuning_df.d <- data.frame(size = 5:10, decay = c(1, 2, 3))
fitControl.d <- trainControl(method = "none")
fitControl.d <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 3)

out.d <- caret::train(x = x, y = y, method = "nnet",
                       trControl = fitControl.d,
                       tuneGrid = tuning_df.d, 
                       maxit = 1000, 
                       MaxNWts = 10000)

pred_error <- prediction_error(x, y, out.nd)
cat("training dataset prediction error", pred_error, "\n")

pred_error <- prediction_error(test.x, test.y, out.nd)
cat("test dataset prediction error", pred_error, "\n")
