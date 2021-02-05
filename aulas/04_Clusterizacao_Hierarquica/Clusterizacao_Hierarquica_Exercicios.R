library(tidyverse)
library(ggdendro)
library(gridExtra)
library(GGally)

# 1

alimentacaoRU <- read.table(file = "AlimentacaoReinoUnido.txt", header = TRUE, sep = "\t")
rownames(alimentacaoRU) <- alimentacaoRU[, 1]
alimentacaoRU <- alimentacaoRU[, -1]

alimentacaoRU <- scale(alimentacaoRU)

# 2

alimentacaoRU.euclidiana <- dist(alimentacaoRU, method = "euclidean")

g1 <- ggdendrogram(hclust(alimentacaoRU.euclidiana, method = "single")) +
  labs(title = "Euclidiana")

# 3

alimentacaoRU.manhattan <- dist(alimentacaoRU, method = "manhattan")

g2 <- ggdendrogram(hclust(alimentacaoRU.manhattan, method = "single")) +
  labs(title = "Manhattan")

grid.arrange(g1, g2, ncol = 2)

# 5

alimentacaoRU <- read.table(file = "AlimentacaoReinoUnido.txt", header = TRUE, sep = "\t")

alimento <- alimentacaoRU[, 1]

alimentacaoRU <- alimentacaoRU[, -1]

alimentacaoRU <- as.data.frame(t(alimentacaoRU))

names(alimentacaoRU) <- alimento

alimentacaoRU <- scale(alimentacaoRU)

alimentacaoRU.euclidiana <- dist(alimentacaoRU, method = "euclidean")

g1 <- ggdendrogram(hclust(alimentacaoRU.euclidiana, method = "single")) +
  labs(title = "Euclidiana")

alimentacaoRU.manhattan <- dist(alimentacaoRU, method = "manhattan")

g2 <- ggdendrogram(hclust(alimentacaoRU.manhattan, method = "single")) +
  labs(title = "Manhattan")

grid.arrange(g1, g2, ncol = 2)

# 6

heptatlo <- read.csv(file = "heptatlo.csv")

heptatlo$Barreiras100m <- 
  max(heptatlo$Barreiras100m)-heptatlo$Barreiras100m
heptatlo$Corrida200m   <- 
  max(heptatlo$Corrida200m)-heptatlo$Corrida200m
heptatlo$Corrida800m   <- 
  max(heptatlo$Corrida800m)-heptatlo$Corrida800m

competidoras <- heptatlo$Nome

heptatlo <- heptatlo %>% 
  select(-c(Nome, Pontos)) %>%
  scale()

rownames(heptatlo) <- competidoras

heptatlo.euclidiana <- dist(heptatlo, method = "euclidean")

g1 <- ggdendrogram(hclust(heptatlo.euclidiana, method = "single")) +
  labs(title = "Euclidiana")

heptatlo.manhattan <- dist(heptatlo, method = "manhattan")

g2 <- ggdendrogram(hclust(heptatlo.manhattan, method = "single")) +
  labs(title = "Manhattan")

grid.arrange(g1, g2, ncol = 2)


###

heptatlo <- read.csv(file = "heptatlo.csv")

heptatlo$Barreiras100m <- 
  max(heptatlo$Barreiras100m)-heptatlo$Barreiras100m
heptatlo$Corrida200m   <- 
  max(heptatlo$Corrida200m)-heptatlo$Corrida200m
heptatlo$Corrida800m   <- 
  max(heptatlo$Corrida800m)-heptatlo$Corrida800m

provas <- names(heptatlo)[-c(1, 9)]

heptatlo <- heptatlo[-c(1,9)]

heptatlo <- t(heptatlo)

heptatlo <- scale(heptatlo)

heptatlo.euclidiana <- dist(heptatlo, method = "euclidean")

g1 <- ggdendrogram(hclust(heptatlo.euclidiana, method = "single")) +
  labs(title = "Euclidiana")

heptatlo.manhattan <- dist(heptatlo, method = "manhattan")

g2 <- ggdendrogram(hclust(heptatlo.manhattan, method = "single")) +
  labs(title = "Manhattan")

grid.arrange(g1, g2, ncol = 2)

