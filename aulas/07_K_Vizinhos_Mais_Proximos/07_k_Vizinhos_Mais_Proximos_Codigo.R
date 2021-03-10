##############
### ajuste ###
##############

# pacotes

library(tidymodels)
theme_set(theme_bw())
library(ggfortify)
library(GGally)

####################
### treino/teste ###
####################

# semente aleatoria

set.seed(1234)

# 75% dos dados como treino

iris_split <- initial_split(iris, prop = .75, strata = Species)

# criar os conjuntos de dados de treino e teste

iris_treino <- training(iris_split)
nrow(iris_treino)/nrow(iris)

iris_teste  <- testing(iris_split)
nrow(iris_teste)/nrow(iris)

# eda

autoplot(prcomp(iris_treino[, -5], center = TRUE, scale. = TRUE), 
         data = iris_treino,
         colour = "Species") +
  scale_colour_viridis_d()

# pre-processamento

iris_rec <- 
  recipe(Species ~ ., 
         data = iris_treino) %>%
  # remover observacoes de modo que todos os niveis de Species
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(Species) %>% 
  # center/scale
  step_center(-Species) %>% 
  step_scale(-Species) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

iris_treino_t <- juice(iris_rec)

# verificar o resultado do processamento de dados

iris_treino_t %>% 
  pivot_longer(-Species) %>% 
  ggplot(., aes(fill = Species)) +
  geom_histogram(aes(value)) +
  facet_wrap(~ name) +
  scale_fill_viridis_d()

ggpairs(iris_treino_t, 
        aes(colour = Species)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

# preparar o conjunto de teste

iris_teste_t <- bake(iris_rec, 
                     new_data = iris_teste)

# definicao do modelo

iris_knn <- 
  nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# criar workflow

iris_wflow <- 
  workflow() %>% 
  add_recipe(iris_rec) %>%
  add_model(iris_knn)

# ajuste do modelo

iris_knn_fit <- fit(iris_wflow, iris_treino_t)



#########################
### validacao cruzada ###
#########################

iris_treino_cv <- vfold_cv(iris_treino_t, v = 5)

iris_treino_cv

# criar workflow

iris_wflow_cv <- 
  workflow() %>% 
  add_model(iris_knn) %>%
  add_formula(Species ~ .)

# rodando validacao cruzada

iris_knn_fit_cv <- 
  iris_wflow_cv %>%
  fit_resamples(iris_treino_cv)

iris_knn_fit_cv

# resultados da cv

collect_metrics(iris_knn_fit_cv)

# resultado final

iris_knn_fit

iris_knn_fit %>% 
  predict(iris_teste_t) %>% 
  bind_cols(iris_teste_t) %>% 
  metrics(truth = Species, estimate = .pred_class)

iris_knn_fit %>%
  # probabilidade para cada classe
  predict(iris_treino_t, type = "prob") %>% 
  bind_cols(iris_treino_t) %>%
  # grafico
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()



##############
### tuning ###
##############

# definicao do tuning

iris_knn_tune <-
  nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>%
  set_engine("kknn") %>%
  set_mode("classification")

iris_knn_tune

# grid de procura

iris_knn_grid <- grid_regular(neighbors(range = c(3, 45)),
                              weight_func(),
                              dist_power(),
                              levels = c(22, 2, 2))

iris_knn_grid

# workflow

iris_knn_tune_wflow <- 
  workflow() %>%
  add_model(iris_knn_tune) %>%
  add_formula(Species ~ .)

# avaliacao do modelo

iris_knn_fit_tune <- 
  iris_knn_tune_wflow %>% 
  tune_grid(
    resamples = iris_treino_cv,
    grid = iris_knn_grid
  )

iris_knn_fit_tune

# resultados

collect_metrics(iris_knn_fit_tune)

iris_knn_fit_tune %>%
  collect_metrics() %>%
  mutate(dist_power = factor(dist_power)) %>%
  ggplot(., aes(x = neighbors, y = mean, color = dist_power)) +
  geom_line() +
  geom_point() +
  facet_grid(weight_func ~ .metric) +
  scale_x_continuous(breaks = seq(3, 45, 6)) +
  scale_colour_viridis_d()

# melhores modelos

iris_knn_fit_tune %>%
  show_best("roc_auc")

iris_knn_fit_tune %>%
  show_best("accuracy")

# melhor modelo

iris_knn_best <- 
  iris_knn_fit_tune %>%
  select_best("accuracy")

iris_knn_final <-
  iris_knn_tune_wflow %>%
  finalize_workflow(iris_knn_best)

iris_knn_final <- fit(iris_knn_final, iris_treino_t)

iris_knn_final

# resultados no conjunto de teste

resultado <- 
  iris_teste_t %>%
  bind_cols(predict(iris_knn_final, iris_teste_t) %>%
              rename(predicao_knn = .pred_class))

metrics(resultado, 
        truth = Species, 
        estimate = predicao_knn,
        options = "roc")


