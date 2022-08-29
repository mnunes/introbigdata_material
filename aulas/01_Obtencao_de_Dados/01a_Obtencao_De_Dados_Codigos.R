###########################
### pacotes necessarios ###
###########################

library(rvest)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(stringr)
library(janitor)

##########################
### obtencao dos dados ###
##########################

# url de interesse

url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"

# baixar os dados localmente e extrair as tabelas da pagina

pagina <- url %>%
  read_html()

pagina <- pagina %>%
  html_table(fill = TRUE)

pilotos <- pagina[[3]]


###############################
### processamento dos dados ###
###############################

head(pilotos)
tail(pilotos)

pilotos <- head(pilotos, -1)

pilotos <- clean_names(pilotos)

head(pilotos)
tail(pilotos)

# limpar a tabela

pilotos$driver_name

pilotos <- 
  pilotos %>%
  mutate(driver_name = str_replace(driver_name, "\\*|\\^|\\~", ""))

pilotos$drivers_championships

pilotos <- 
  pilotos %>%
  mutate(drivers_championships = as.numeric(as.character(substring(drivers_championships, 1, 1))))
  

for (j in 5:11){
  pilotos[, j] <- as.numeric(as.character(str_replace(as.matrix(pilotos[, j]), "\\[.*\\]", "")))
}

names(pilotos) <- c("nome", "pais", "temporadas", "campeonatos", "inscricoes", "largadas", "poles", "vitorias", "podios", "voltas_mais_rapidas", "pontos")



#########################
### analise dos dados ###
#########################

# 1

pilotos %>%
  select(nome, pais, campeonatos) %>%
  arrange(desc(campeonatos)) %>%
  filter(campeonatos >=  1)

# 2

pilotos %>%
  group_by(pais) %>%
  summarise(campeonatos_por_pais = sum(campeonatos)) %>%
  arrange(desc(campeonatos_por_pais)) %>%
  filter(campeonatos_por_pais >=  1)

# 3

ggplot(pilotos, aes(x = poles, y = vitorias)) +
  geom_point()

# 4

ggplot(pilotos, aes(x = poles, y = vitorias)) +
  geom_point(aes(colour = as.factor(campeonatos))) +
  labs(colour = "Campeonatos")


