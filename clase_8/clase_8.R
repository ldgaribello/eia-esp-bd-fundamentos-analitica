setwd("~/Workspace/Study/Fundamentos Analitica/clase_8")
getwd()

# install.packages("libcoin")
# nstall.packages("inum")

#### Load libraries  ####
library(C50)

# Post-prunning library
library(rpart)
library(rpart.plot)

####   Load data     ####
credit <- read.csv("credit.csv", stringsAsFactors = T)

# Verification
str(credit)

# Transformation


#### Data analitics  ####
summary(credit)

prop.table(table(credit$default))

# Entrenamiento y testeo #1
set.seed(1234)

index.train <- sample(1:nrow(credit), floor(0.8 * nrow(credit)))
credit.train <- credit[ index.train, -17] 
credit.test  <- credit[-index.train, -17]

label.train <- credit[ index.train, 17]
label.test  <- credit[-index.train, 17]

# Algoritmo #1
class.dt <- C50::C5.0(credit.train, label.train)
summary(class.dt)
# Predicción
pred.dt <- predict(class.dt, credit.test)
table(pred.dt, label.test)
mean(pred.dt == label.test)

# Algoritmo #2
class.dt2 <-C50::C5.0(credit.train[, 1:3], label.train)
summary(class.dt2)
# Predicción
pred.dt2 <- predict(class.dt2, credit.test)
table(pred.dt2, label.test)
mean(pred.dt2 == label.test)

# Algoritmo #3 - Pre-prunning
class.dt3 <-C50::C5.0(credit.train, label.train,
                      control = C50::C5.0Control(noGlobalPruning = T,
                                                 minCases = 5))
summary(class.dt3)
# Predicción
pred.dt3 <- predict(class.dt3, credit.test)
table(pred.dt3, label.test)
mean(pred.dt3 == label.test)

# Algoritmo #4 - Post-prunning
credit.posp.train <-  credit[ index.train, ] 
credit.posp.test  <-  credit[-index.train, ]

set.seed(1234)
class.dt4 <- rpart(default ~ ., 
                   data = credit.posp.train,
                   method = "class")
plotcp(class.dt4)
printcp(class.dt4)
rpart.plot(class.dt4)

mat.cp <-  as.data.frame(class.dt4$cptable)
mat.cp$CP[which(mat.cp$xerror == min(mat.cp$xerror))]
mat.cp$CP

class.dt4.pruned <- prune(class.dt4, cp = 0.019364)
# Predicción
pred.dt4 <- predict(class.dt4.pruned, credit.test, type = "class")
table(pred.dt4, label.test)
mean(pred.dt4 == label.test)

####     Results     ####
