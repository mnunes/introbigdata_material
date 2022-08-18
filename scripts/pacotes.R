# script para instalacao dos pacotes necessarios para a 
# disciplina EST0133 - Introducao a Modelagem de Big Data.
# este script encontra os pacotes instalados na maquina 
# do usuario e baixa e instala apenas aqueles que estao
# faltando.
# 
# autor: Marcus Nunes 
# site:  https://marcusnunes.me

# repositorio de pacotes

options(repos = c(CRAN = "http://cran.rstudio.com"))

# lista de pacotes necessarios

pacotes.necessarios <- c("AppliedPredictiveModeling",
                         "caret", 
                         "caretEnsemble", 
                         "e1071", 
                         "Ecdat",
                         "factoextra", 
                         "GGally", 
                         "ggdendro", 
                         "ggfortify", 
                         "gridExtra", 
                         "janitor", 
                         "kknn", 
                         "lubridate", 
                         "magick",
                         "mclust",
                         "mlbench", 
                         "modeldata", 
                         "NbClust", 
                         "onehot", 
                         "palmerpenguins", 
                         "pROC", 
                         "randomForest", 
                         "rvest", 
                         "scales", 
                         "stringr", 
                         "themis", 
                         "tidymodels", 
                         "tidyverse",
                         "vip")

# instalacao dos pacotes que faltam na maquina

pacotes.novos <- pacotes.necessarios[!(pacotes.necessarios %in% installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(pacotes.novos, dependencies = TRUE)
  print("##########################")
  print("### Pacotes instalados ###")
  print("##########################")
} 

# atualizacao dos pacotes jah instalados

update.packages(ask = FALSE)

print("###########################")
print("### Pacotes atualizados ###")
print("###########################")
