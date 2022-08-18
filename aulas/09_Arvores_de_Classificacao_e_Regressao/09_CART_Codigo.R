library(rpart)
modelo <- rpart(Species ~ ., 
                method = "class", 
                data = iris)

print(modelo)

library(rattle)

fancyRpartPlot(modelo, caption = NULL)



###############
### exemplo ###
###############

# pacotes

library(tidymodels)
theme_set(theme_bw())
library(vip)


####################
### treino/teste ###
####################

# semente aleatoria

set.seed(4236)

# 70% dos dados como treino

iris_split <- initial_split(iris, prop = .70, strata = Species)

# criar os conjuntos de dados de treino e teste

iris_treino <- training(iris_split)
nrow(iris_treino)/nrow(iris)

iris_teste  <- testing(iris_split)
nrow(iris_teste)/nrow(iris)

# eda

autoplot(prcomp(iris_treino %>% select(-Species), 
                center = TRUE, scale. = TRUE), 
         data = iris_treino,
         colour = "Species") +
  scale_colour_viridis_d()

iris_treino %>%
  pivot_longer(-Species) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ name)

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

# preparar o conjunto de teste

iris_teste_t <- bake(iris_rec,
                     new_data = iris_teste)



##############
### tuning ###
##############

#####################
# definicao do tuning

iris_rpart_tune <-
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(),
    min_n = tune()) %>%
  set_engine("rpart") %>% 
  set_mode("classification")

# grid de procura

iris_rpart_grid <- grid_regular(cost_complexity(range(-5, -1)),
                                tree_depth(range(1, 5)),
                                min_n(range(1, 11)),
                                levels = c(5, 5, 3))

iris_rpart_grid

# workflow

iris_rpart_tune_wflow <- 
  workflow() %>%
  add_model(iris_rpart_tune) %>%
  add_formula(Species ~ .)

# definicao da validacao cruzada

set.seed(1740)

iris_treino_cv <- vfold_cv(iris_treino_t, v = 5)

# avaliacao do modelo

iris_rpart_fit_tune <- 
  iris_rpart_tune_wflow %>% 
  tune_grid(
    resamples = iris_treino_cv,
    grid = iris_rpart_grid
  )

# resultados

collect_metrics(iris_rpart_fit_tune)

iris_rpart_fit_tune %>%
  collect_metrics() %>%
  mutate(cost = cost_complexity,
         depth = factor(tree_depth)) %>%
  ggplot(., aes(x = cost, y = mean, colour = depth, group = depth)) +
  geom_line() +
  geom_point() +
  facet_grid(min_n ~ .metric) +
  scale_x_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_viridis_d()

# melhores modelos

iris_rpart_fit_tune %>%
  show_best("roc_auc")

iris_rpart_fit_tune %>%
  show_best("accuracy")

# melhor modelo

iris_rpart_best <- 
  iris_rpart_fit_tune %>%
  select_best("accuracy")

iris_rpart_final <-
  iris_rpart_tune_wflow %>%
  finalize_workflow(iris_rpart_best)

iris_rpart_final <- fit(iris_rpart_final, 
                        iris_treino_t)

iris_rpart_final

# resultados no conjunto de teste

resultado_rpart <- 
  iris_teste_t %>%
  bind_cols(predict(iris_rpart_final, iris_teste_t) %>%
              rename(predicao_rpart = .pred_class))

metrics(resultado_rpart, 
        truth = Species, 
        estimate = predicao_rpart,
        options = "roc")

conf_mat(resultado_rpart, 
         truth = Species, 
         estimate = predicao_rpart) %>%
  autoplot(type = "heatmap")

sens(resultado_rpart, 
     truth = Species, 
     estimate = predicao_rpart)

spec(resultado_rpart, 
     truth = Species, 
     estimate = predicao_rpart)

iris_rpart_final %>% 
  extract_fit_parsnip() %>% 
  vip(scale = TRUE)
