# 1

library(tidyverse)
theme_set(theme_bw())
library(ggfortify)

# 2

pokemon <- read.csv(file="Pokemon.csv", na.strings = "")

str(pokemon)

# 3

pokemon %>%
  group_by(Type.1) %>%
  count() %>%
  arrange(desc(n))

# 4

ggplot(pokemon, aes(x=reorder(Type.1, Attack, FUN=median), y=Attack)) +
  geom_boxplot() +
  labs(x="Tipo de Pokemon", y="Ataque") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 5

ggplot(pokemon, aes(x=Defense, y=Attack)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="lm", se=FALSE)

ajuste <- lm(Attack ~ Defense, data=pokemon)

summary(ajuste)

autoplot(ajuste)

pokemon[231, ]
predict(ajuste, newdata=data.frame(Defense=230))

pokemon[430, ]
predict(ajuste, newdata=data.frame(Defense=20))



#

library(rvest)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(stringr)
library(scales)

# 1

url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o"

populacao <- url %>%
  read_html()

populacao <- populacao %>%
  html_table(fill=TRUE)

populacao <- populacao[[1]]

names(populacao) <- c("Posição", "Código do IBGE", "Município", "Unidade federativa", "População")

head(populacao)
tail(populacao)

# 2

url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_brasileiros_por_%C3%A1rea_decrescente"

area <- url %>%
  read_html()

area <- area %>%
  html_table(fill=TRUE)

area <- area[[1]]

head(area)
tail(area)

# 3

# utilizar `Município` e `Unidade federativa` nao funciona porque
# algumas cidades estao com grafias difernetes nos dois data frames

dados <- left_join(populacao, area, by = c("Município", "Unidade federativa"))

head(dados)


# 4

dados <- dados %>%
  select(Município, `Unidade federativa`, `Área (km²)`, População)

names(dados) <- c("municipio", "estado", "area", "populacao")

head(dados)

dados$area <- str_replace(dados$area, "[[:space:]]", "")
dados$area <- str_replace(dados$area, ",", ".")
dados$area <- as.numeric(dados$area)

dados$populacao <- str_replace_all(dados$populacao, "[[:space:]]", "")
dados$populacao <- as.numeric(dados$populacao)

head(dados)

str(dados)

dados <- na.omit(dados)

# 5

ggplot(dados, aes(x=area, y=populacao)) +
  geom_point() +
  labs(x="Área (km^2)", y="População")

ggplot(dados, aes(x=area, y=populacao)) +
  geom_point() +
  labs(x="log(Área (km^2))", y="População") +
  scale_x_log10(labels=comma)

ggplot(dados, aes(x=area, y=populacao)) +
  geom_point() +
  labs(x="Área (km^2)", y="log(População)") +
  scale_y_log10(labels=comma)

ggplot(dados, aes(x=area, y=populacao)) +
  geom_point() +
  labs(x="log(Área (km^2))", y="log(População)") +
  scale_y_log10(labels=comma) +
  scale_x_log10(labels=comma)

# 6

dados %>%
  arrange(desc(populacao)) %>%
  head(5)

dados %>%
  arrange(desc(populacao)) %>%
  tail(5)

# 7

dados <- dados %>%
  mutate(densidade=populacao/area)

dados %>%
  arrange(desc(densidade)) %>%
  head(5)

dados %>%
  arrange(desc(densidade)) %>%
  tail(5)

dados %>%
  filter(municipio=="Natal")

dados %>%
  filter(municipio=="Macaíba")

# 8

dados %>%
  filter(estado=="Rio Grande do Norte") %>%
  arrange(desc(populacao)) %>%
  head(5)

dados %>%
  filter(estado=="Rio Grande do Norte") %>%
  arrange(desc(populacao)) %>%
  tail(5)

# 9

dados %>%
  filter(estado=="Rio Grande do Norte") %>%
  arrange(desc(densidade)) %>%
  head(5)

dados %>%
  filter(estado=="Rio Grande do Norte") %>%
  arrange(desc(densidade)) %>%
  tail(5)


