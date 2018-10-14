# setwd("~/Desktop/HW/bis557/data-raw/")
ridge_train <- read.csv("ridge_train.csv")
save(ridge_train, file = "../data/ridge_train.rda")

ridge_test <- read.csv("ridge_test.csv")
save(ridge_test, file = "../data/ridge_test.rda")
