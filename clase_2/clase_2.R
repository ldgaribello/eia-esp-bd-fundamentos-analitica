setwd("~/Workspace/Study/Fundamentos Analitica/playground")

#### Load libraries  ####
# install.packages("ggmap")
library(ggmap)

####   Load data     ####
murder <- subset(crime, offense=="murder")
murder

#### Data analitics  ####
qmplot(lon, lat, data = murder, 
       colour = I("red"), size = I(2))

####     Results     ####
