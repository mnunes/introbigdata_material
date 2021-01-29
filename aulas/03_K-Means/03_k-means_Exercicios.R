library(GGally)
library(tidyverse)
theme_set(theme_bw())
library(ggfortify)
library(factoextra)
library(NbClust)
library(gridExtra)

# 1

vendas <- read.csv(file="vendas.csv")

ggpairs(vendas)

summary(vendas)

vendas.pca <- prcomp(vendas, center=TRUE, scale.=TRUE)

summary(vendas.pca)

autoplot(vendas.pca, loadings=TRUE, loadings.label=TRUE)

# 2

fviz_nbclust(vendas, kmeans, method="wss")

# 3

fviz_nbclust(vendas, kmeans, method="silhouette")

# 2 correto

fviz_nbclust(scale(vendas), kmeans, method="wss")

# 3 correto

fviz_nbclust(scale(vendas), kmeans, method="silhouette")

# 4

cluster_4 <- kmeans(scale(vendas), 4)$cluster

g4 <- autoplot(vendas.pca) +
  geom_point(aes(colour=as.factor(cluster_4))) +
  labs(colour="Clusters")

cluster_5 <- kmeans(scale(vendas), 5)$cluster

g5 <- autoplot(vendas.pca) +
  geom_point(aes(colour=as.factor(cluster_5))) +
  labs(colour="Clusters")

cluster_6 <- kmeans(scale(vendas), 6)$cluster

g6 <- autoplot(vendas.pca) +
  geom_point(aes(colour=as.factor(cluster_6))) +
  labs(colour="Clusters")

cluster_7 <- kmeans(scale(vendas), 7)$cluster

g7 <- autoplot(vendas.pca) +
  geom_point(aes(colour=as.factor(cluster_7))) +
  labs(colour="Clusters")

grid.arrange(g4, g5, g6, g7)

# 5

heptatlo <- read.csv(file="heptatlo.csv")

names(heptatlo)

heptatlo$Barreiras100m <- 
  max(heptatlo$Barreiras100m)-heptatlo$Barreiras100m
heptatlo$Corrida200m   <- 
  max(heptatlo$Corrida200m)-heptatlo$Corrida200m
heptatlo$Corrida800m   <- 
  max(heptatlo$Corrida800m)-heptatlo$Corrida800m

heptatlo.novo <- heptatlo %>% 
  select(-c(Nome, Pontos))

heptatlo.pca <- prcomp(heptatlo.novo, center=TRUE, scale.=TRUE)
summary(heptatlo.pca)

ggpairs(heptatlo.novo)

autoplot(heptatlo.pca)

fviz_nbclust(scale(heptatlo.novo), kmeans, method="wss")
fviz_nbclust(scale(heptatlo.novo), kmeans, method="silhouette")

cluster_2 <- kmeans(scale(heptatlo.novo), 2)$cluster

g2 <- autoplot(heptatlo.pca) +
  geom_point(aes(colour=as.factor(cluster_2))) +
  labs(colour="Clusters")

cluster_3 <- kmeans(scale(heptatlo.novo), 3)$cluster

g3 <- autoplot(heptatlo.pca) +
  geom_point(aes(colour=as.factor(cluster_3))) +
  labs(colour="Clusters")

cluster_4 <- kmeans(scale(heptatlo.novo), 4)$cluster

g4 <- autoplot(heptatlo.pca) +
  geom_point(aes(colour=as.factor(cluster_4))) +
  labs(colour="Clusters")

cluster_5 <- kmeans(scale(heptatlo.novo), 5)$cluster

g5 <- autoplot(heptatlo.pca) +
  geom_point(aes(colour=as.factor(cluster_5))) +
  labs(colour="Clusters")

grid.arrange(g2, g3, g4, g5)


