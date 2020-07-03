setwd("~/Workspace/Study/Fundamentos Analitica/clase_12")
getwd()

# K-Means


#### Load libraries  ####
# install.packages("factoextra")
# install.packages("ggsignif")
# install.packages("rstatix")
library(factoextra)
library(cluster)

####   Load data     ####
sns <- read.csv("snsdata.csv")
str(sns)

# Transformaciones
sns <-  sns[!is.na(sns$age) & !is.na(sns$gender), ]

# Common Functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#### Data analitics  ####
sns.clu <-  sns[, 5:40]
sns.clu <-  as.data.frame(apply(sns.clu, 2, normalize))

set.seed(1234)
clusters <- kmeans(sns.clu, 3)
summary(clusters)

sns$cluster <- clusters$cluster
sns$mujer   <- as.numeric(sns$gender == "F")

aggregate(sns$mujer, by=list(sns$cluster), mean)

factoextra::fviz_cluster(clusters, data = sns.clu)

# Clusterizaci´on jerarquica
attach(USArrests)
arrest <- USArrests
View(arrest)

arrest <- as.data.frame(apply(arrest, 2, normalize))

# AGNES
clu.agnes <- cluster::agnes(arrest)

pltree(clu.agnes)
plot(clu.agnes)
cutree(clu.agnes, k = 5)
rect.hclust(clu.agnes, k=5)
