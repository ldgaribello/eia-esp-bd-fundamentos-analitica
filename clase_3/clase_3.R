setwd("~/Workspace/Study/Fundamentos Analitica/playground")

update.packages()

#### Load libraries  ####
install.packages("swirl")
library(swirl)

####   Load data     ####
murder <- subset(crime, offense=="murder")
murder

#### Data analitics  ####
graph.map <- qmplot(lon, lat, data = murder, 
       colour = I("red"), size = I(2))

nom <- "Leon"
ape <- "Garibello"
paste(nom, ape, sep = " ")

demo(graphics)

ls()
dir()

vignette("grid")

x <- c(1, 2, 3, 4, NA)
sum(x, na.rm = T)
mean(x, na.rm = T)
var(x)
sd(x)

#l1 <- c(-5,-2,0,2,5)
#l2 <- l2 ^ 2

l1 <- c(1,3,4,6,8)
l2 <- c(2,3,5,7,9)
cor(l1, l2)
cov(l1, l2)

zc <- cbind(l1, l2)
zr <- rbind(l1, l2)
class(zc)

# Secuencias
c(1:10)
2 + c(1:10)
seq(1, 10, 0.5)
seq(from = 1, to = 10, length = 19)

a1 <- c("a", "b", "a")
a1
class(a1)
a1_f <- factor(a1)
a1_f
class(a1_f)

x <- c(1, 3, -2)
y <- c("a", "a", "b")
z <- data.frame(x, y)
z
class(z)

x <- c(1, 3, -2)
is.numeric(x)
as.character(x)

x <- c("1", "3", "-2")
is.character(x)
as.numeric(x)

x <- factor(c(1, 0, 1, 1))
x
is.numeric(x)
as.numeric(x)
as.numeric(as.character(x))


x <- 1:10<5
class(x)
as.numeric(x)
which(x)

x <- c(1, 3, -2)
y <- c("a", "a", "b")
y_f <- factor(c("a", "a", "b"))
summary(x)
summary(y)
summary(y_f)
range(x)
range(y)

x <- matrix(1:30, 3, 10, byrow = T)
class(x)
x
x[2:3, 1:2]
x[c(1,3)]
dim(x)
nrow(x)
ncol(x)
as.vector(x)

####     Results     ####
