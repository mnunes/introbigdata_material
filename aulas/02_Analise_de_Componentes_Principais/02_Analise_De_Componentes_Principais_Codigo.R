##
library(ggplot2)
theme_set(theme_bw())

##
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(x = "Velocidade (mph)", y = "Distância (pés)")

##
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  labs(x = "Comprimento da Sépala", y = "Largura da Sépala")

##
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  labs(x = "Comprimento da Pétala", y = "Largura da Pétala")

##
library(GGally)
ggpairs(iris[, -5])

##
iris.pca <- prcomp(iris[, -5], center = TRUE, scale. = TRUE)
names(iris.pca)
head(iris.pca$x)

##
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(colour = Species)) +
  labs(x = "Comprimento da Pétala", y = "Largura da Pétala", 
  colour = "Espécies")

##
summary(iris.pca)

##
plot(iris.pca)

##
iris.transformado <- data.frame(iris.pca$x, 
  Species = iris$Species)
head(iris.transformado)

##
ggplot(iris.transformado, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Species)) +
  labs(x = "Componente Principal 1", y = "Componente Principal 2", 
  colour = "Espécies")

##
library(ggfortify)
autoplot(iris.pca, data = iris, colour = "Species")

##
autoplot(iris.pca, data = iris, colour = "Species", loadings = TRUE, loadings.label = TRUE)

##
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)

heptatlo <- read.csv(file = "heptatlo.csv")

names(heptatlo)

##

transformacao <- function(x){
  return(max(x) - x)
}

heptatlo <- heptatlo %>%
  mutate(Barreiras100m = transformacao(Barreiras100m)) %>%
  mutate(Corrida200m   = transformacao(Corrida200m)) %>%
  mutate(Corrida800m   = transformacao(Corrida800m))

## 
heptatlo.novo <- heptatlo %>% 
  select(-c(Nome, Pontos))

##
ggpairs(heptatlo.novo)

##
corrplot(cor(heptatlo.novo), method = "ellipse")

##
heptatlo.pca <- prcomp(heptatlo.novo, center = TRUE, scale. = TRUE)
summary(heptatlo.pca)

##
plot(heptatlo.pca)

##
corrplot(cor(heptatlo.pca$x), method = "ellipse")

##
heptatlo.transformado <- data.frame(heptatlo.pca$x, Nome = heptatlo$Nome)
ggplot(heptatlo.transformado, aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_text(aes(label = Nome), alpha = 0.35) +
  xlim(-4, 7)



