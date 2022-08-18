library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(ggfortify)

# 1

iris.pca <- prcomp(iris[, -5], center=TRUE, scale.=TRUE)
names(iris.pca)

summary(iris.pca)

plot(iris.pca)

iris.transformado <- data.frame(iris.pca$x, Species=iris$Species)
head(iris.transformado)

pca01 <- ggplot(iris.transformado, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=Species)) +
  labs(colour="Especies")

pca02 <- ggplot(iris.transformado, aes(x=PC1, y=PC3)) +
  geom_point(aes(colour=Species)) +
  labs(colour="Especies")

pca03 <- ggplot(iris.transformado, aes(x=PC2, y=PC4)) +
  geom_point(aes(colour=Species)) +
  labs(colour="Especies")

pca04 <- ggplot(iris.transformado, aes(x=PC3, y=PC4)) +
  geom_point(aes(colour=Species)) +
  labs(colour="Especies")

grid.arrange(pca01, pca02, pca03, pca04)

pca01 <- autoplot(iris.pca, data=iris, x=1, y=2, colour="Species")

pca02 <- autoplot(iris.pca, data=iris, x=1, y=3, colour="Species")

pca03 <- autoplot(iris.pca, data=iris, x=1, y=4, colour="Species")

pca04 <- autoplot(iris.pca, data=iris, x=3, y=4, colour="Species")

grid.arrange(pca01, pca02, pca03, pca04)

# 2

autoplot(iris.pca, data=iris, colour="Species", loadings=TRUE, loadings.label=TRUE)

autoplot(iris.pca, data=iris, x=3, y=4, colour="Species", loadings=TRUE, loadings.label=TRUE)

# 3

ggplot(iris.transformado, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=Species)) +
  geom_text(aes(label=Species, colour=Species)) +
  labs(colour="Especies")

# 4

alimentacao <- read.table(file="AlimentacaoReinoUnido.txt", sep="\t", header=TRUE)

# 5

alimentacao.novo <- alimentacao %>%
  select(-Alimento)

alimentacao.pca <- prcomp(alimentacao.novo, center = TRUE, scale. = TRUE)

# 6

summary(alimentacao.pca)

# 7

plot(alimentacao.pca)

# 8

alimentacao.transformada <- data.frame(alimentacao.pca$x, Alimento=alimentacao$Alimento)

ggplot(alimentacao.transformada, aes(x=PC1, y=PC2)) +
  geom_point() +
  geom_text(aes(label=Alimento), alpha=0.35)

# 9

autoplot(alimentacao.pca, data=alimentacao, loadings=TRUE, loadings.label=TRUE)

# 10

rownames(alimentacao.novo) <- alimentacao$Alimento

alimentacao.pca.transposta <- prcomp(t(alimentacao.novo), center=TRUE, scale. = TRUE)

summary(alimentacao.pca.transposta)

plot(alimentacao.pca.transposta)

alimentacao.transformada <- data.frame(alimentacao.pca.transposta$x, Paises=names(alimentacao)[2:5])

ggplot(alimentacao.transformada, aes(x=PC1, y=PC2)) +
  geom_point() +
  geom_text(aes(label=Paises))

autoplot(alimentacao.pca.transposta, data=t(alimentacao.novo), label=TRUE, loadings=TRUE, loadings.label=TRUE)


