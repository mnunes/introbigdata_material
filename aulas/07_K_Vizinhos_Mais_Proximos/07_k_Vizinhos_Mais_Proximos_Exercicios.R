#########
### 1 ###
#########

library(tidymodels)
theme_set(theme_bw())
library(ggfortify)
library(mlbench)
data(Sonar)

sonar <- 
  Sonar %>%
  rename(class = Class)

# pca

autoplot(prcomp(sonar[, -61], center = TRUE, scale. = TRUE), 
         data = sonar,
         colour = "class") +
  scale_colour_viridis_d()



#########
### 2 ###
#########

# semente aleatoria

set.seed(1234)

# 70% dos dados como treino

sonar_split <- initial_split(sonar, prop = .7, strata = class)

# criar os conjuntos de dados de treino e teste

sonar_treino <- training(sonar_split)
nrow(sonar_treino)/nrow(sonar)

sonar_teste  <- testing(sonar_split)
nrow(sonar_teste)/nrow(sonar)



#########
### 2 ###
#########

# pre-processamento

sonar_rec <- 
  recipe(class ~ ., 
         data = sonar_treino) %>%
  # remover observacoes de modo que todos os niveis de class
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(class) %>% 
  # center/scale
  step_center(-class) %>% 
  step_scale(-class) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

sonar_treino_t <- juice(sonar_rec)

# verificar o resultado do processamento de dados

sonar_treino_t %>% 
  pivot_longer(-class) %>% 
  mutate(name = factor(name, 
                       levels = paste0("V", 1:60))) %>%
  ggplot(., aes(fill = class)) +
  geom_histogram(aes(value)) +
  facet_wrap(~ name) +
  scale_fill_viridis_d()

# preparar o conjunto de teste

sonar_teste_t <- bake(sonar_rec, 
                      new_data = sonar_teste)

# validacao cruzada 

set.seed(9999)

sonar_treino_cv <- vfold_cv(sonar_treino_t, v = 7)

sonar_treino_cv

# grid de procura

sonar_knn_grid <- grid_regular(neighbors(range = c(3, 19)),
                               weight_func(),
                               dist_power(),
                               levels = c(9, 2, 2))

# definicao do modelo

sonar_knn <-
  nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# criar workflow

sonar_wflow_cv <- 
  workflow() %>% 
  add_model(sonar_knn) %>%
  add_formula(class ~ .)

# avaliacao do modelo

sonar_knn_fit_tune <- 
  sonar_wflow_cv %>% 
  tune_grid(
    resamples = sonar_treino_cv,
    grid = sonar_knn_grid
  )

sonar_knn_fit_tune

# resultados da cv

collect_metrics(sonar_knn_fit_tune)

# comparacao de modelos

sonar_knn_fit_tune %>%
  collect_metrics() %>%
  mutate(dist_power = factor(dist_power)) %>%
  ggplot(., aes(x = neighbors, y = mean, color = dist_power)) +
  geom_line() +
  geom_point() +
  facet_grid(weight_func ~ .metric) +
  scale_x_continuous(breaks = seq(3, 19, 2)) +
  scale_colour_viridis_d()

# melhores modelos

sonar_knn_fit_tune %>%
  show_best("roc_auc")

sonar_knn_fit_tune %>%
  show_best("accuracy")

# melhor modelo

sonar_knn_best <- 
  sonar_knn_fit_tune %>%
  select_best("roc_auc")

sonar_knn_final <-
  sonar_wflow_cv %>%
  finalize_workflow(sonar_knn_best)

sonar_knn_final <- fit(sonar_knn_final, sonar_treino_t)

sonar_knn_final



#########
### 4 ###
#########

sonar_knn_fit_tune %>%
  show_best("roc_auc", 1)

sonar_knn_fit_tune %>%
  show_best("accuracy", 1)

sonar_knn_final %>% 
  predict(sonar_teste_t) %>% 
  bind_cols(sonar_teste_t) %>% 
  metrics(truth = class, estimate = .pred_class)



#########
### 5 ###
#########

sonar_knn_final %>%
  # probabilidade para cada classe
  predict(sonar_treino_t, type = "prob") %>% 
  bind_cols(sonar_treino_t) %>%
  select(.pred_M, .pred_R, class) %>%
  #print(n = Inf) %>%
  # grafico
  roc_curve(class, .pred_M) %>%
  autoplot()

