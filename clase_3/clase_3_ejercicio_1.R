setwd("~/Workspace/Study/Fundamentos Analitica/clase_3")

#### Load libraries  ####


####   Load data     ####


#### Data analitics  ####

# E1: Generar un vector columna, 
#     distribucion normal, 
#     media = 10
#     varianza = 5
x <- rnorm(50, 10, sqrt(5)) # Tercer parametro es la desviacion estandar
x 

#E2
y <- seq(from = -5, length = 20, by = 0.5)
y

#E3
# z <- cbind(x, xy)
z <- c(x1, x2)
z

#E4
#w <- runif(70, min(z), max(z))
w <- runif(70)
wz <- cbind(z, w)
wz

#E5
wz2 <- wz[(w[,2]>.2 & w[,2]<.4) | (w[,2]>.6 & w[,2]<.8)]

#E6
paste(
  "La media de la colimna 1 es: ", mean(wz2[,1]), " ",
  "Y la varianza es: ", var(w2[,1])
)
  
####     Results     ####
