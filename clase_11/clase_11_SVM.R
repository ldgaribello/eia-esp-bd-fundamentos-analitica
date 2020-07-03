setwd("~/Workspace/Study/Fundamentos Analitica/clase_11")
getwd()

# SVM

#### Load libraries  ####
library(e1071)

####   Load data     ####
attach(iris)

# Common Functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#### Data analitics  ####

# Entrenamiento y testeo
set.seed(1234)

index.train <- sample(1:nrow(iris), 
                      floor(0.8 * nrow(iris)))
iris.train <- iris[ index.train, ] 
iris.test  <- iris[-index.train, ]

# Implementar algoritmo
mod.svm <- svm(Species ~ . , data = iris.train)
mod.svm

pred.svm = predict(mod.svm, iris.test)
table(pred.svm, iris.test$Species)
mean(pred.svm == iris.test$Species)

plot(mod.svm, 
     iris.test,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width  = mean(iris$Sepal.Width),
                  Sepal.Length = mean(iris$Sepal.Length)))

# Tuning kernel y parametro
set.seed(1234)
mod.svm.tune = tune(svm, 
                    train.x = iris.train[, -5], 
                    train.y = iris.train[,  5],
                    kernel  = "radial",
                    ranges  = list(cost=c(1, 2), gamma   = c(.5, 1)))

set.seed(1234)
mod.svm.tune2 = tune(svm, 
                     train.x = iris.train[, -5], 
                     train.y = iris.train[,  5],
                     kernel  = "radial",
                     ranges  = list(cost  = seq(.1, 5, 0.2), 
                                    gamma = seq(.1, 2, 0.1)))
plot(mod.svm.tune2)

mod.svm.tune2$best.model
pred.svm.best <- predict(mod.svm.tune2$best.model, iris.test[, -5])
table(pred.svm.best, iris.test$Species)
mean(pred.svm.best == iris.test$Species)

####     Results     ####

