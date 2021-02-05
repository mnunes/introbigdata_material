library(tidyverse)
library(ggdendro)
library(factoextra)

# preparacao dos dados

prisoes <- scale(USArrests)

prisoes.euclidiana <- dist(prisoes, method = "euclidean")

prisoes.manhattan  <- dist(prisoes, method = "manhattan")

# dendogramas

ggdendrogram(hclust(prisoes.euclidiana, method = "single")) +
  labs(title = "Dist = Euclidiana, Link = Simples")

ggdendrogram(hclust(prisoes.euclidiana, method = "complete")) +
  labs(title = "Dist = Euclidiana, Link = Completo")

ggdendrogram(hclust(prisoes.euclidiana, method = "ward.D")) +
  labs(title = "Dist = Euclidiana, Link = Ward")

ggdendrogram(hclust(prisoes.manhattan, method = "single")) +
  labs(title = "Dist = Manhattan, Link = Simples")

ggdendrogram(hclust(prisoes.manhattan, method = "complete")) +
  labs(title = "Dist = Manhattan, Link = Completo")

ggdendrogram(hclust(prisoes.manhattan, method = "ward.D")) +
  labs(title = "Dist = Manhattan, Link = Ward")

# determinacao do numero de clusters

euclidiana_single <- function(x, k) {
  list(cluster = cutree(hclust(dist(x), 
                               method = "single"), 
                        k=k))
  }

fviz_nbclust(prisoes, 
             FUNcluster = euclidiana_single, 
             method = "gap_stat", 
             nboot = 100)

euclidiana_complete <- function(x, k) {
  list(cluster = cutree(hclust(dist(x), 
                               method = "complete"), 
                        k=k))
}

fviz_nbclust(prisoes, 
             FUNcluster = euclidiana_complete, 
             method = "gap_stat", 
             nboot = 100)

euclidiana_ward <- function(x, k) {
  list(cluster = cutree(hclust(dist(x), 
                               method = "ward.D"), 
                        k=k))
}

fviz_nbclust(prisoes, 
             FUNcluster = euclidiana_ward, 
             method = "gap_stat", 
             nboot = 100)
