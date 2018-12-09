setwd("/users/mateuszzezula/Box/1. First Semester/Programming for Data Science/Homework/homework_13")

library(pacman)
p_load(dplyr, caret)


show_number <- function(m, i, oriented=T)
{
  im <- matrix(mtrain[i,], byrow=T, nrow=28)

  if (oriented) {
    im_orient <- matrix(0, nrow=28, ncol=28)
    for (i in 1:28)
      im_orient[i,] <- rev(im[,i])

    im <- im_orient
  }
  image(im)
}

if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header = F) %>% as.matrix
  train_classification <- mtrain[, 1]
  y <- factor(train_classification, levels = c(0, 1))
  # for caret, y variable should be a factor
  # see line 54 in caret_intro_2d.R
  mtrain <- mtrain[,-1]/256  # x matrix
  colnames(mtrain) <- 1:(28^2)
  x <- mtrain[1:1000,]
}

toy_data <- read.csv("toy_data_2d_train1.csv", header=T)
toy_data$y <- factor(toy_data$y, levels=c(0, 1))

mtrain2 <- read.csv("mnist_train.csv", header = F) %>% as.matrix
mtrain3 <- mtrain2

mtrain3[,1] <- if (mtrain2[,1] != 3) {
  mtrain2[,1] == 0
} else {
  mtrain2[,1] == 1
}

mtrain <- read.csv("mnist_train.csv", header = F)
mtrain$V1 <- as.numeric(mtrain[,1] == 3)
mtrain$V1 <- factor(mtrain$V1, levels = c(0, 1))
mtrain <- as.matrix(mtrain)

y <- as.numeric(mtrain[, 1] == 3)
mtrain[,1] <- as.factor(y)
mtrain[,1] <- as.factor(mtrain[,1])


# toy_data <- read.csv("toy_data_2d_train1.csv", header=T)
mtrain3$y <- factor(mtrain3$y, levels = c(0, 1))


###Making the mtrain matrix###
if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header=F) %>% as.matrix
  train_classification <- mtrain[,1] #Go through this vector, set equal to zero if not 3 and 1 if 3. Gives y values
  mtrain <- mtrain[,-1]/256 #x values
  
  
  colnames(mtrain) <- 1:(28^2)
  #colnames(mtrain) <- NULL
  rownames(mtrain) <- NULL
  
  x <- mtrain[1:1000,]
}
y <- rep(NA, length(train_classification))

#Converting all threes to one and all other numbers to zero


mtrain_test <- read.csv("mnist_train.csv", header = F) %>% as.matrix
train_classification <- mtrain_test[, 1]
y <- factor(train_classification, levels = c(0, 1))

for (i in 1:length(train_classification)){
  cn <- train_classification[i]
  if (cn==3){
    cn <- 1
  } else {
    cn <- 0
  }
  y[i] <- cn
}

y <- factor(y, levels=c(0,1))
y <- y[1:1000]

