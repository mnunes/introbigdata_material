library(mvtnorm)
library(ggplot2)
theme_set(theme_bw())

##
set.seed(1)

N     <- 200
mu1   <- c(0, 0)
mu2   <- c(1.5, 1.5)
Sigma <- matrix(c(1, 0, 0, 1), ncol = 2)

ctrl <- rmvnorm(n = N, mean = mu1, sigma = Sigma)
trt  <- rmvnorm(n = N, mean = mu2, sigma = Sigma)

dados <- data.frame(rbind(ctrl, trt), rep(c("Controle", "Tratamento"), each = N))
names(dados) <- c("x1", "x2", "Grupo")

ggplot(dados, aes(x = x1, y = x2)) + 
  geom_point(aes(colour = Grupo))

class <- kmeans(dados[, 1:2], 2)
names(class)

head(class$cluster)
sum(class$cluster[1:N] == 1)/N
sum(class$cluster[(N+1):(2*N)] == 2)/N

class$centers
class$size

dados$Cluster <- as.factor(class$cluster)
ggplot(dados, aes(x = x1, y = x2)) + 
  geom_point(aes(shape = Grupo, colour = Cluster))


##

set.seed(1)

N     <- 200
mu1   <- c(0, 0)
mu2   <- c(2.5, 2.5)
Sigma <- matrix(c(1, 0, 0, 1), ncol = 2)

ctrl <- rmvnorm(n = N, mean = mu1, sigma = Sigma)
trt  <- rmvnorm(n = N, mean = mu2, sigma = Sigma)

dados <- data.frame(rbind(ctrl, trt), rep(c("Controle", "Tratamento"), each = N))
names(dados) <- c("x1", "x2", "Grupo")

ggplot(dados, aes(x = x1, y = x2)) + 
  geom_point(aes(colour = Grupo))

class <- kmeans(dados[, 1:2], 2)
names(class)

head(class$cluster)
sum(class$cluster[1:N] == 1)/N
sum(class$cluster[(N+1):(2*N)] == 2)/N

class$centers
class$size

dados$Cluster <- as.factor(class$cluster)
ggplot(dados, aes(x = x1, y = x2)) + 
  geom_point(aes(shape = Grupo, colour = Cluster))


##

library(factoextra)
library(NbClust)

x <- scale(iris[, 1:4])

fviz_nbclust(x, kmeans, method = "wss")

fviz_nbclust(x, kmeans, method = "silhouette")


##

x <- scale(iris[, 1:4])

iris.kmeans <- kmeans(x, centers = 3)

iris.pca <- prcomp(x, center = TRUE, scale. = TRUE)

iris.plot <- data.frame(x, 
                        iris.pca$x, 
                        Species = iris$Species,
                        Clusters = as.character(iris.kmeans$cluster))

ggplot(iris.plot, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(shape = Species, colour = Clusters)) +
  labs(x = "Comprimento da Petala", y = "Largura da Petala")

ggplot(iris.plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Species, colour = Clusters)) +
  labs(x = "PC1", y = "PC2")



