# 1

library(tidyverse)
theme_set(theme_bw())
library(ggfortify)

# 2

pokemon <- read.csv(file = "Pokemon.csv", na.strings = "")

str(pokemon)

# 3

pokemon %>%
  group_by(Type.1) %>%
  count() %>%
  arrange(desc(n))

# 4

ggplot(pokemon, aes(x = reorder(Type.1, Attack, FUN = median), y = Attack)) +
  geom_boxplot() +
  labs(x = "Tipo de Pokemon", y = "Ataque") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 5

ggplot(pokemon, aes(x = Defense, y = Attack)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE)

ajuste <- lm(Attack ~ Defense, data = pokemon)

summary(ajuste)

autoplot(ajuste)

pokemon[231, ]
predict(ajuste, newdata = data.frame(Defense = 230))

pokemon[430, ]
predict(ajuste, newdata = data.frame(Defense = 20))



#

library(rvest)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(stringr)
library(scales)
library(janitor)

# 1

url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o"

populacao <- url %>%
  read_html()

populacao <- populacao %>%
  html_table(fill = TRUE)

populacao <- populacao[[1]]

populacao <- clean_names(populacao)

head(populacao)
tail(populacao)

# 2

url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_brasileiros_por_%C3%A1rea_decrescente"

area <- url %>%
  read_html()

area <- area %>%
  html_table(fill = TRUE)

area <- area[[1]]

area <- clean_names(area)

head(area)
tail(area)

area <- area %>%
  rename(codigo_ibge = codigo_do_ibge)

# 3

# utilizar `Município` e `Unidade federativa` nao funciona porque
# algumas cidades estao com grafias difernetes nos dois data frames

dados <- left_join(populacao, area, by = "codigo_ibge")

head(dados)

# 4

dados <- dados %>%
  select(municipio = municipio.x, 
         estado = unidade_federativa.x, 
         area = area_km2, 
         populacao)

head(dados)

dados <- 
  dados %>%
  mutate(area = str_replace(area, "[[:space:]]", "")) %>%
  mutate(area = str_replace(area, ",", ".")) %>%
  mutate(area = as.numeric(area))

dados <- 
  dados %>%
  mutate(populacao = str_replace_all(populacao, "[[:space:]]", "")) %>%
  mutate(populacao = as.numeric(populacao))

head(dados)

str(dados)

dados <- na.omit(dados)

# 5

ggplot(dados, aes(x = area, y = populacao)) +
  geom_point() +
  labs(x = "Área (km^2)", y = "População")

ggplot(dados, aes(x = area, y = populacao)) +
  geom_point() +
  labs(x = "log(Área (km^2))", y = "População") +
  scale_x_log10(labels = comma)

ggplot(dados, aes(x = area, y = populacao)) +
  geom_point() +
  labs(x = "Área (km^2)", y = "log(População)") +
  scale_y_log10(labels = comma)

ggplot(dados, aes(x = area, y = populacao)) +
  geom_point() +
  labs(x = "log(Área (km^2))", y = "log(População)") +
  scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma)

# 6

dados %>%
  arrange(desc(populacao)) %>%
  head(5)

dados %>%
  arrange(desc(populacao)) %>%
  tail(5)

# 7

dados <- dados %>%
  mutate(densidade = populacao/area)

dados %>%
  arrange(desc(densidade)) %>%
  head(5)

dados %>%
  arrange(desc(densidade)) %>%
  tail(5)

dados %>%
  filter(municipio == "Natal")

dados %>%
  filter(municipio == "Parnamirim")

# 8

dados %>%
  filter(estado == "Rio Grande do Norte") %>%
  arrange(desc(populacao)) %>%
  head(5)

dados %>%
  filter(estado == "Rio Grande do Norte") %>%
  arrange(desc(populacao)) %>%
  tail(5)

# 9

dados %>%
  filter(estado == "Rio Grande do Norte") %>%
  arrange(desc(densidade)) %>%
  head(5)

dados %>%
  filter(estado == "Rio Grande do Norte") %>%
  arrange(desc(densidade)) %>%
  tail(5)


