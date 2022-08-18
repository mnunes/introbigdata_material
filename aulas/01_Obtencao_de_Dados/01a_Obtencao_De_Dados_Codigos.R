###########################
### pacotes necessarios ###
###########################

library(rvest)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(stringr)

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

pilotos <- 
  pilotos %>%
  rename(Name = `Driver Name`,
         Championships = `Drivers' Championships`)

# limpar a tabela

pilotos$Name
pilotos$Name <- str_replace(pilotos$Name, "\\*|\\^|\\~", "")

pilotos$Championships
pilotos$Championships <- as.numeric(
  as.character(substring(pilotos$Championships, 1, 1)))

for (j in 5:10){
  pilotos[, j] <- as.numeric(as.character(str_replace(as.matrix(pilotos[, j]), "\\[.*\\]", "")))
}

names(pilotos) <- c("Nome", "Pais", "Temporadas", "Campeonatos", "Inscricoes", "Largadas", "Poles", "Vitorias", "Podios", "VoltasMaisRapidas", "Pontos")



#########################
### analise dos dados ###
#########################

# 1

pilotos %>%
  select(Nome, Pais, Campeonatos) %>%
  arrange(desc(Campeonatos)) %>%
  filter(Campeonatos >=  1)

# 2

pilotos %>%
  group_by(Pais) %>%
  summarise(Campeonatos_por_Pais = sum(Campeonatos)) %>%
  arrange(desc(Campeonatos_por_Pais)) %>%
  filter(Campeonatos_por_Pais >=  1)

# 3

ggplot(pilotos, aes(x = Poles, y = Vitorias)) +
  geom_point()

# 4

ggplot(pilotos, aes(x = Poles, y = Vitorias)) +
  geom_point(aes(colour = as.factor(Campeonatos))) +
  labs(colour = "Campeonatos")


