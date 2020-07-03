setwd("~/Workspace/Study/Fundamentos Analitica/clase_10")
getwd()


#### Load libraries  ####
library(psych)
# install.packages("margins")
library(margins)

source("Logitplot.R")

####   Load data     ####
insurance <-  read.csv("insurance.csv", stringsAsFactors = T)

# Validation
names(insurance)
str(insurance)

# Transformation
# Pacientes en el 25% superior de gasto medico
insurance$pac.costo <-  as.numeric(insurance$expenses >= summary(insurance$expenses)[5])

#### Data analitics  ####
summary(insurance$pac.costo)

prop.table(table(insurance$pac.costo, insurance$sex), 1)
prop.table(table(insurance$pac.costo, insurance$sex), 2)
chisq.test(table(insurance$pac.costo, insurance$sex))

prop.table(table(insurance$pac.costo, insurance$smoker), 1)
prop.table(table(insurance$pac.costo, insurance$smoker), 2)
chisq.test(table(insurance$pac.costo, insurance$smoker))

# Entrenamiento y testeo
set.seed(1234)

index.train <- sample(1:nrow(insurance), floor(0.8 * nrow(insurance)))
insurance.train <- insurance[ index.train, ] 
insurance.test  <- insurance[-index.train, ]

# Algoritmo -  Regresion Logistica
reg.mod <- glm(pac.costo ~ . - expenses,
               family = binomial(link = "logit"),
               data = insurance.train)
summary(reg.mod)

# Marginal effects
margins::margins(reg.mod)

pred.reg <- predict(reg.mod, insurance.test, type = "response")
pred.cl <-  ifelse(pred.reg > 0.5, 1, 0)

table(pred.cl, insurance.test$pac.costo)
mean(pred.cl == insurance.test$pac.costo)

# cutpoint
roc.stats <- logit.roc(reg.mod, 200)

# Balance especificidad y sensibilidad
cut.balance <- roc.stats$pts[abs(roc.stats$sens - roc.stats$spec) == 
  min(abs(roc.stats$sens - roc.stats$spec))]

logit.plot.ss(logit.roc(reg.mod, 200))

# Recalcular con nuevo C
pred.cl <-  ifelse(pred.reg > cut.balance, 1, 0)
table(pred.cl, insurance.test$pac.costo)
mean(pred.cl == insurance.test$pac.costo)

# Maximizar precisión
cut.off <- seq(.1, .9, length.out = 200)
acc <- matrix(NA, length(cut.off))

for (i in 1:length(cut.off)) {
  pred.cl <-  ifelse(pred.reg > cut.off[i], 1, 0)
  acc[i, 1] <- mean(pred.cl == insurance.test$pac.costo)
}

acc[acc == max(acc)]
cut.off[acc == max(acc)][1]
pred.cl <- ifelse(pred.reg > cut.off[acc == max(acc)][1], 1, 0)
table(pred.cl, insurance.test$pac.costo)
####     Results     ####
