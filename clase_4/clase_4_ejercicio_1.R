setwd("~/Workspace/Study/Fundamentos Analitica/clase_4")
getwd()

# Ejercicio:
# Divida la base de datos en 4 grupos de igual tamaño 
# de acuerdo a la variable 'mpg' 
# Y estime descripciones estadisticas para 'cyl' y 'hp' 
# Y exporte los resultados a excel

#### Load libraries  ####


####   Load data     ####
data("mtcars")

#### Data analitics  ####
class(mtcars)

# Descriptivas de mtcars
summary(mtcars)

mtcars_desc <- list()
# Valor del limite del tercer quartil
mtcars_desc$mpg_Q3 = summary(mtcars$mpg)[5]
# Valor de la mediana
mtcars_desc$mpg_median = summary(mtcars$mpg)[3]
# Valor del limite del primer quartil
mtcars_desc$mpg_Q1 = summary(mtcars$mpg)[2]
mtcars_desc

mtcars$gmpg <- 4
mtcars$gmpg[mtcars$mpg < mtcars_desc$mpg_Q3] <- 3
mtcars$gmpg[mtcars$mpg < mtcars_desc$mpg_median] <- 2
mtcars$gmpg[mtcars$mpg < mtcars_desc$mpg_Q1] <- 1

####     Results     ####
mtcars$gmpg
mtcars[c("mpg", "gmpg")]

table(mtcars["gmpg"])

output <- aggregate(mtcars[c("cyl", "hp")], 
                    by = list(mtcars$gmpg),
                    FUN = mean)

write.csv(output, file = "mtcars_descriptive.csv")
