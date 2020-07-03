setwd("~/Workspace/Study/Fundamentos Analitica/clase_11")
getwd()

# Neural Networks

#### Load libraries  ####
library(neuralnet)
library(NeuralNetTools)

####   Load data     ####
concrete <-  read.csv("concrete.csv", stringsAsFactors = T)

# Validation
names(concrete)
str(concrete)
head(concrete)

# Common Functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#### Data analitics  ####
cor(concrete)

# Parametro 2: 1 - Filas, 2 -> Columnas
concrete.norm <- as.data.frame(apply(concrete, 2, normalize))
class(concrete.norm)

# Entrenamiento y testeo
set.seed(1234)

index.train <- sample(1:nrow(concrete.norm), floor(0.8 * nrow(concrete.norm)))
concrete.train <- concrete.norm[ index.train, ] 
concrete.test  <- concrete.norm[-index.train, ]

# Implementar algoritmo
set.seed(1234)
mod.nn <- neuralnet::neuralnet(strength ~ . , data = concrete.train)

summary(mod.nn)
names(mod.nn)
plot(mod.nn)

pred.nn <- predict(mod.nn, concrete.test)

mean((concrete.test$strength - pred.nn) ^ 2)

# ECM en unidades originales
yobs <- concrete[-index.train, "strength"]
ypred <- pred.nn * (max(concrete$strength) - min(concrete$strength)) +
  min(concrete$strength)
ecm.1 <-  mean((yobs - ypred) ^ 2)  
  
# Modelo con capa ocultas
set.seed(1234)
mod.nn2 <- neuralnet::neuralnet(strength ~ . , 
                                data = concrete.train,
                                hidden = 4)

pred.nn2 <- predict(mod.nn2, concrete.test)

mean((concrete.test$strength - pred.nn2) ^ 2)

# ECM en unidades originales
yobs <- concrete[-index.train, "strength"]
ypred2 <- pred.nn2 * (max(concrete$strength) - min(concrete$strength)) +
  min(concrete$strength)
ecm.2 <-  mean((yobs - ypred2) ^ 2)  
ecm.2 / ecm.1

# Multiples capas ocultas
set.seed(1234)
mod.nn3 <- neuralnet::neuralnet(strength ~ . , 
                                data = concrete.train,
                                hidden = c(5, 3))

pred.nn3 <- predict(mod.nn3, concrete.test)

mean((concrete.test$strength - pred.nn3) ^ 2)

# ECM en unidades originales
yobs <- concrete[-index.train, "strength"]
ypred3 <- pred.nn3 * (max(concrete$strength) - min(concrete$strength)) +
  min(concrete$strength)
ecm.3 <-  mean((yobs - ypred3) ^ 2)  
ecm.3 / ecm.1

####     Results     ####

