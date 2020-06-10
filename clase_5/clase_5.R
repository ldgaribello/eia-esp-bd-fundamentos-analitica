setwd("~/Workspace/Study/Fundamentos Analitica/clase_5")
getwd()

#### Load libraries  ####
library(fastDummies)
library(plyr)

# Alternativas para la descripción de datos
# install.packages("Hmisc")
# install.packages("xfun")
library(Hmisc)
# install.packages("pastecs")
library(pastecs)
# install.packages("psych")
library(psych)

####   Load data     ####
data("mtcars")
?mtcars

Hmisc::describe(mtcars)
pastecs::stat.desc(mtcars)
psych::describe(mtcars)

#### Data analitics  ####
x = mtcars 

?ave
x$mean.mpg.cyl <- ave(x$mpg, x$cyl, FUN = mean)
x$median.mpg.cyl <- ave(x$mpg, x$cyl, FUN = median)
x$summary.q1.mpg.cyl <- ave(x$mpg, 
                            x$cyl, 
                            FUN = function(p) summary(p)[2])

x[c("mpg", "cyl", "mean.mpg.cyl")]
x[c("mpg", "cyl", "median.mpg.cyl")]
x[c("mpg", "cyl", "summary.q1.mpg.cyl")]
x[c("mpg", "cyl", "median.mpg.cyl", "summary.q1.mpg.cyl")]

mean(x$mpg[x$cyl == 6])
median(x$mpg[x$cyl == 6])

summary(x)
summary(x[c("cyl")])
summary(x$mpg)
summary(x$mpg[x["cyl"] == 4])
summary(x$mpg[x["cyl"] == 6])
summary(x$mpg[x["cyl"] == 8])

cyl.dummies <-  dummy_cols(as.factor(x$cyl))




stat.mpg <- ddply(x, .(cyl, vs), summarize,
                  mean = round(mean(mpg), 2),
                  sd = round(sd(mpg), 2))
stat.mpg

data.by.cyl <- ddply(x, .(cyl), summarize,
                  mpg = max(mpg),
                  vs = mean(vs),
                  am = mean(am))
data.by.cyl

# Variables continuas
plot(x$mpg, x$hp)

plot(x$mpg, x$hp, xlab = "MPG", ylab = "HP",
     main = "My first R plot",
     sub = "This is fun!",
     col = "red", 
     col.axis = "blue", 
     col.lab = "purple", 
     col.sub = "darkgreen",
     fg = "yellow4")

hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)

mpg_density = density(mtcars$mpg)
plot(mpg_density)
polygon(mpg_density, col = "red", border = "black")

boxplot(mtcars$mpg)

# Variables categoricas
counts <- list()
counts.gear = table(mtcars$gear)
barplot(counts.gear,
        main = "Car Distribution",
        xlab = "Number of Gears",
        ylim = c(0, 16))

counts.vs.gear = table(mtcars$vs, mtcars$gear)
barplot(counts.vs.gear,
        main = "Count by vs and gear",
        xlab = "Number of Gears",
        col = c("darkblue", "red"),
        legend = rownames(counts.vs.gear),
        ylim = c(0, 16),
        beside = TRUE)

counts.vs = table(mtcars$vs)
pie(counts.vs, main = "VS of cars")
pie(prop.table(counts.vs), 
    labels = paste(prop.table(counts.vs) * 100, "%"),
    main = "VS of cars")

# Análisis bivariado

# Tablas de contingencia 
table_am_vs <- table(mtcars$am, mtcars$vs)

margin.table(table_am_vs, 1)
margin.table(table_am_vs, 2)

# Tabla de contingencia
prop.table(table_am_vs)

# Distribución acumulada
prop.table(table_am_vs, 1)

# Test de independecia de varirables
# h0 = independnecia
# p-valor 0,05
chisq.test(table_am_vs)

####     Results     ####


