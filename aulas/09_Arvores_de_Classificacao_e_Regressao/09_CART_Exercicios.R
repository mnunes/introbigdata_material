# pacotes

library(MASS)
library(tidymodels)
theme_set(theme_bw())
library(GGally)
library(ggfortify)
library(vip)

# 1

# juntar os conjuntos de dados originais

pima <- 
  bind_rows(Pima.tr, Pima.te) %>%
  mutate(type = relevel(type, ref = "Yes"))

pima %>%
  group_by(type) %>%
  count()


# 2

####################
### treino/teste ###
####################

# semente aleatoria

set.seed(4236)

# 75% dos dados como treino

pima_split <- initial_split(pima, prop = .75, strata = type)

# criar os conjuntos de dados de treino e teste

pima_treino <- training(pima_split)
nrow(pima_treino)/nrow(pima)

pima_teste  <- testing(pima_split)
nrow(pima_teste)/nrow(pima)

# eda

autoplot(prcomp(pima_treino %>% select(-type), 
                center = TRUE, scale. = TRUE), 
         data = pima_treino,
         colour = "type") +
  scale_colour_viridis_d()

pima_treino %>%
  pivot_longer(-type) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ name)

# como vamos trabalhar com CART, nao iremos transformar
# as variaveis do conjunto de dados


# 3

# pre-processamento

pima_rec <- 
  recipe(type ~ ., 
         data = pima_treino) %>%
  # remover observacoes de modo que todos os niveis de type
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(type) %>% 
  # center/scale
  step_center(-type) %>% 
  step_scale(-type) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

pima_treino_t <- juice(pima_rec)

# verificar o resultado do processamento de dados

pima_treino_t %>% 
  pivot_longer(-type) %>% 
  ggplot(., aes(fill = type)) +
  geom_histogram(aes(value)) +
  facet_wrap(~ name) +
  scale_fill_viridis_d()

# preparar o conjunto de teste

pima_teste_t <- bake(pima_rec,
                     new_data = pima_teste)



##############
### tuning ###
##############

#####################
# definicao do tuning

pima_rpart_tune <-
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(),
    min_n = tune()) %>%
  set_engine("rpart") %>% 
  set_mode("classification")

# grid de procura

pima_rpart_grid <- grid_regular(cost_complexity(range(-5, -1)),
                                tree_depth(range(1, 5)),
                                min_n(range(1, 11)),
                                levels = c(5, 5, 3))

pima_rpart_grid

# workflow

pima_rpart_tune_wflow <- 
  workflow() %>%
  add_model(pima_rpart_tune) %>%
  add_formula(type ~ .)

# definicao da validacao cruzada

set.seed(1740)

pima_treino_cv <- vfold_cv(pima_treino_t, v = 8)

# avaliacao do modelo

pima_rpart_fit_tune <- 
  pima_rpart_tune_wflow %>% 
  tune_grid(
    resamples = pima_treino_cv,
    grid = pima_rpart_grid
  )

# resultados

collect_metrics(pima_rpart_fit_tune)

pima_rpart_fit_tune %>%
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

pima_rpart_fit_tune %>%
  show_best("roc_auc")

pima_rpart_fit_tune %>%
  show_best("accuracy")

# melhor modelo

pima_rpart_best <- 
  pima_rpart_fit_tune %>%
  select_best("accuracy")

pima_rpart_final <-
  pima_rpart_tune_wflow %>%
  finalize_workflow(pima_rpart_best)

pima_rpart_final <- fit(pima_rpart_final, 
                        pima_treino_t)

pima_rpart_final



# 4

# resultados no conjunto de teste

resultado_rpart <- 
  pima_teste_t %>%
  bind_cols(predict(pima_rpart_final, pima_teste_t) %>%
              rename(predicao_rpart = .pred_class))

metrics(resultado_rpart, 
        truth = type, 
        estimate = predicao_rpart,
        options = "roc")

conf_mat(resultado_rpart, 
         truth = type, 
         estimate = predicao_rpart) %>%
  autoplot(type = "heatmap")

sens(resultado_rpart, 
     truth = type, 
     estimate = predicao_rpart)

spec(resultado_rpart, 
     truth = type, 
     estimate = predicao_rpart)

# 5

pima_rpart_final %>% 
  extract_fit_parsnip() %>% 
  vip(scale = TRUE)

