setwd("~/Workspace/Study/Fundamentos Analitica/clase_8")
getwd()

# install.packages("OneR")
# install.packages("arulesCBA")

#### Load libraries  ####
library(OneR)
library(arulesCBA)

####   Load data     ####
mushrooms <-  read.csv("mushrooms.csv", stringsAsFactors = T)

# Verificacion
str(mushrooms)

# Transformacion
mushrooms$veil_type <-  NULL

#### Data analitics  ####
table(mushrooms$type)

# Entrenamiento y testeo #1
set.seed(1234)

index.train <- sample(1:nrow(mushrooms), floor(0.8 * nrow(mushrooms)))
mushrooms.train <- mushrooms[ index.train, ] 
mushrooms.test  <- mushrooms[-index.train, ]

# Algoritmo OneR
class.cr <- OneR::OneR(type ~ .,
                       data = mushrooms.train)
pred.cr <- predict(class.cr, mushrooms.test)
table(pred.cr, mushrooms.test$type)
mean(pred.cr == mushrooms.test$type)

class.cr2 <- OneR::OneR(type ~ cap_shape + population + cap_color,
                       data = mushrooms.train)
pred.cr2 <- predict(class.cr2, mushrooms.test)
table(pred.cr2, mushrooms.test$type)
mean(pred.cr2 == mushrooms.test$type)

# Algoritmo CBA
class.cr3 = CBA(type ~ .,
                data = mushrooms.train)
rules(class.cr3)
inspect(rules(class.cr3))

pred.cr3 <- predict(class.cr3, mushrooms.test)
table(pred.cr3, mushrooms.test$type)
mean(pred.cr3 == mushrooms.test$type)

# Algoritmo CBA #2
mushrooms.train.cba <- mushrooms.train[, c("type", 
                                           "cap_shape",
                                           "population",
                                           "cap_color")]
mushrooms.test.cba  <-  mushrooms.test[, c("type", 
                                            "cap_shape",
                                            "population",
                                            "cap_color")]
class.cr4 = CBA(type ~ .,
                data = mushrooms.train.cba)
rules(class.cr4)
inspect(rules(class.cr4))

pred.cr4 <- predict(class.cr4, mushrooms.test.cba)
table(pred.cr4, mushrooms.test.cba$type)
mean(pred.cr4 == mushrooms.test.cba$type)


# Algoritmo CBA #2
mushrooms.train.cba2 <- mushrooms.train[, c("type", 
                                           "cap_shape",
                                           "population",
                                           "odor")]
mushrooms.test.cba2  <-  mushrooms.test[, c("type", 
                                           "cap_shape",
                                           "population",
                                           "odor")]
class.cr5 = CBA(type ~ .,
                data = mushrooms.train.cba2)
rules(class.cr5)
inspect(rules(class.cr5))

pred.cr5 <- predict(class.cr5, mushrooms.test.cba2)
table(pred.cr5, mushrooms.test.cba2$type)
mean(pred.cr5 == mushrooms.test.cba2$type)