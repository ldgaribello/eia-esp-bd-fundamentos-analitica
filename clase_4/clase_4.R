setwd("~/Workspace/Study/Fundamentos Analitica/clase_4")
getwd()

#### Load libraries  ####
install.packages("ggfortify")
install.packages("dplyr")
library(ggfortify)
library(dplyr)

####   Load data     ####
data("austres")
data("mtcars")

#### Data analitics  ####
austres
mtcars

# Visualización (Austres: Una sola serie)
View(austres)
View(mtcars)
# Análisis Descriptivo
summary(austres)
summary(mtcars)

autoplot(austres)
#autoplot(mtcars)

head(mtcars)
str(mtcars)
names(mtcars)

mtcars[mtcars$am==1,]
mtcars[mtcars$am==1, 2:5]

mtcars[c(TRUE, FALSE)]
mtcars$mpg
mtcars[,c("mpg", "vs")]
mtcars[,1]

?dim
dim(mtcars)
mtcars$hp[dim(mtcars)[1]]

rownames(mtcars)
colnames(mtcars)

nombre.carros <- row.names(mtcars)
nombre.carros[mtcars$am==1]

attach(mtcars)
wt
detach(mtcars)

# Time Series
x <- c(1, 2, 4, 5, 4, 6)
x.ts <- ts(x, frequency = 12, start = c(2020,1))
x.ts

y <- mtcars[mtcars$am==1,]
y$hcyl <- y$cyl>4
y

# Crear la variable hcyl pero con formato numerico
# Crear una variable que tome el valor de 1 si el carro 
# Tienen un desempeño por encima del promedio (mpg)
y$hmpg <- as.numeric(y$mpg > mean(y$mpg))
y$cyl.f <- as.factor(y$cyl)
summary(y)
levels(y$cyl.f) <- c("Bajo", "Medio", "Alto")

mtcars2 <- mtcars[order(mtcars$mpg),]
mtcars2 <- mtcars[order(-mtcars$mpg),]

mtcars[sample(1:length(mtcars[,1]), 30),]
mtcars[sample(1:length(mtcars[,1]), 30, replace = TRUE),]

subdata <- select(mtcars, 2:3)
subdata <- select(mtcars, c(mpg, cyl, disp, hp, drat, wt))
subdata <- select(mtcars, -c(mpg, cyl, disp, hp, drat, wt))

subdata <- filters(mtcars, cyl==6)
subdata <- filters(mtcars, cyl==6 & hp==110)

c(mean(mtcars$mpg), mean(mtcars$hp))

stats.cars <- apply(mtcars[c("mpg", "gear")], 2, summary)
summary(mtcars[c("mpg", "gear")])
# sapply, #lapply

apply(mtcars[c("mpg", "gear")], 2, function(x) mean(x))

stats.cars
aggregate(mtcars[c("mpg", "gear")], by=list(mtcars$cyl), 
          FUN = mean)
aggregate(mtcars[c("mpg", "gear")], by=list(mtcars$cyl), 
          FUN = summary)

####     Results     ####
write.csv(summary(mtcars), file = "stats.csv")
