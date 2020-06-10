setwd("~/Workspace/Study/Fundamentos Analitica/clase_7")
getwd()

#### Load libraries  ####
library(e1071)


####   Load data     ####
data(iris)

# Validation
head(iris)
str(iris)


# Common Functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#### Data analitics  ####
summary(iris$Species)

pairs(iris[1:4], 
      pch = 21, 
      bg = c("red", "green", "blue")[unclass(iris$Species)])

# Entrenamiento y testeo #1
set.seed(1234)

index.train <- sample(1:nrow(iris), floor(0.8*nrow(iris)))
iris.train <- iris[index.train, 1:4] 
iris.test  <- iris[-index.train, 1:4]

label.train <- iris[index.train, 5]
label.test  <- iris[-index.train, 5]

# Algoritmo
class.nb <- e1071::naiveBayes(iris.train, label.train)
# plot(density(iris$Sepal.Length))

pred.nb <- predict(class.nb, iris.test)
table(pred.nb, label.test)
mean(pred.nb == label.test)


# Entrenamiento y testeo #2
iris.c = iris[, 1:2]
iris.c$Sepal.Length <- cut(iris.c$Sepal.Length,
                           c(0, mean(iris$Sepal.Length), Inf),
                           labels = c("Low", "High"))
iris.c$Sepal.Width <- cut(iris.c$Sepal.Width,
                          c(0, mean(iris$Sepal.Width), Inf),
                          labels = c("Low", "High"))

iris.c.train <- iris.c[index.train, ] 
iris.c.test  <- iris.c[-index.train, ]

# Algoritmo
class.nb2 <- e1071::naiveBayes(iris.c.train, label.train) 
pred.nb2 <- predict(class.nb2, iris.c.test)
table(pred.nb2, label.test)
mean(pred.nb2 == label.test)

class.nb3 <- e1071::naiveBayes(iris.c.train, label.train, laplace = 1) 
pred.nb3 <- predict(class.nb3, iris.c.test)
table(pred.nb3, label.test)
mean(pred.nb3==label.test)

# NB usando kernels
library(naivebayes)
nb4.dens <- naive_bayes(iris.c.train, 
                        label.train, 
                        usekernel = T)
pred.nb4 <- predict(nb4.dens, 
                    iris.c.test)
table(pred.nb4, label.test)
mean(pred.nb4 == label.test)

# Ejemplo NB usando matrices conteo
data(UCBAdmissions)
class(UCBAdmissions)

class.nb5 <- naiveBayes(Admit ~ Gender + Dept, 
                       data = UCBAdmissions)
predict(class.nb5, data.frame(Gender="Female", Dept="B"))

####     Results     ####

