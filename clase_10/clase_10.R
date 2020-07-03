setwd("~/Workspace/Study/Fundamentos Analitica/clase_10")
getwd()

#### Load libraries  ####
library(psych)

####   Load data     ####
insurance <-  read.csv("insurance.csv", stringsAsFactors = T)

# Validation
names(insurance)
str(insurance)

#### Data analitics  ####
summary(insurance$expenses)
hist(insurance$expenses)
boxplot(insurance$expenses)

hist(log(insurance$expenses))
boxplot(log(insurance$expenses))

prop.table(table(insurance$sex))
prop.table(table(insurance$region))

cor(insurance[c("age", "bmi", "children", "expenses")])
psych::pairs.panels(insurance[c("age", "bmi", "children", "expenses")])


# Entrenamiento y testeo
set.seed(1234)

index.train <- sample(1:nrow(insurance), floor(0.8 * nrow(insurance)))
insurance.train <- insurance[ index.train, ] 
insurance.test  <- insurance[-index.train, ]

# Algoritmo - Regresion Lineal
reg.mod <- lm(expenses ~ ., data = insurance.train)
summary(reg.mod)

pred.reg <- predict(reg.mod, insurance.test)
plot(pred.reg, insurance.test$expenses)

# Error cuadratico medio
ecm.1 <- mean((insurance.test$expenses-pred.reg) ^ 2)

# Transformaciones del modelo
# Edad tienen un efecto positivo y creciente: incluir polinomio orden 2
reg.mod2 <- lm(expenses ~ . + I(age ^ 2), data = insurance.train)
summary(reg.mod2)

pred.reg2 <- predict(reg.mod2, insurance.test)
plot(pred.reg2, insurance.test$expenses)

# Error cuadratico medio
ecm.2 <- mean((insurance.test$expenses-pred.reg2) ^ 2)
ecm.2 / ecm.1

# Interaccion entre edad y bmi
reg.mod3 <- lm(expenses ~ . + I(age ^ 2) + I(age*bmi), data = insurance.train)
summary(reg.mod3)

pred.reg3 <- predict(reg.mod3, insurance.test)
plot(pred.reg3, insurance.test$expenses)

# Error cuadratico medio
ecm.3 <- mean((insurance.test$expenses-pred.reg3) ^ 2)
ecm.3 / ecm.1


# Interaccion entre edad y bmi con fumador
reg.mod4 <- lm(expenses ~ . + I(age ^ 2) + smoker*bmi, data = insurance.train)
summary(reg.mod4)

pred.reg4 <- predict(reg.mod4, insurance.test)
plot(pred.reg4, insurance.test$expenses)

# Error cuadratico medio
ecm.4 <- mean((insurance.test$expenses-pred.reg4) ^ 2)
ecm.4 / ecm.1

####     Results     ####

