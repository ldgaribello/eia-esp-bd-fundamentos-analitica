setwd("~/Workspace/Study/Fundamentos Analitica/clase_6")

#### Load libraries  ####


####   Load data     ####
dataknn <-  read.csv("wisc_bc_data.csv")
head(dataknn)
names(dataknn)
str(dataknn)

#### Data analitics  ####
dataknn$id <- NULL
dataknn$diagnosis <-as.factor(dataknn$diagnosis)

# Cantidad de B/M
table(dataknn$diagnosis)

# Porcentaje de B/M
prop.table(table(dataknn$diagnosis))

# radius_mean, area_mean, concavity_mean
aggregate(dataknn[c("radius_mean", 
                    "area_mean", 
                    "concavity_mean")], 
          by=list(dataknn$diagnosis),
          FUN = mean)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1,2,3,4))
normalize(c(-1,1,2))

dataknn.norm <-  apply(dataknn[, -1], 2, normalize)
#dataknn.norm <-  apply(dataknn[, -1], 2, scale)

# Entrenamiento y testeo
set.seed(1234)

index.train <- sample(1:nrow(dataknn), floor(0.8*nrow(dataknn)))

dataknn.train <- dataknn.norm[index.train,] 
dataknn.test  <- dataknn.norm[-index.train,]

label.train <- dataknn[index.train, 1]
label.test  <- dataknn[-index.train, 1]

# Algoritmo
library(class)
dataknn.prediction <- knn(train = dataknn.train, 
                          test = dataknn.test, 
                          cl= label.train,
                          k= floor(sqrt(nrow(dataknn.train))))
table(dataknn.prediction, label.test)
mean(dataknn.prediction == label.test)

dataknn.prediction <- knn(train = dataknn.train, 
                          test = dataknn.test, 
                          cl= label.train,
                          k= 24)
table(dataknn.prediction, label.test)
mean(dataknn.prediction == label.test)



kval <- 10:50
acc.k <- matrix(NA, length(kval)) 

for (i in 1:length(kval)) {
  dataknn.prediction <- knn(train = dataknn.train, 
                            test = dataknn.test, 
                            cl= label.train,
                            k= kval[i])
  acc.k[i] <- mean(dataknn.prediction == label.test)
}

plot(acc.k)

kval[acc.k == max(acc.k)]
acc.k[acc.k == max(acc.k)]

####     Results     ####

