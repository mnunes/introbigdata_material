# pacotes carregados

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

set.seed(7202)

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

# como vamos trabalhar com random forest, nao iremos transformar
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

pima_rf_tune <-
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# grid de procura

pima_rf_grid <- grid_regular(mtry(range(1, 7)),
                             min_n(range(10, 50)),
                             levels = c(7, 5))

pima_rf_grid

# workflow

pima_rf_tune_wflow <- 
  workflow() %>%
  add_model(pima_rf_tune) %>%
  add_formula(type ~ .)

# definicao da validacao cruzada

set.seed(3343)

pima_treino_cv <- vfold_cv(pima_treino_t, v = 8)

# avaliacao do modelo

pima_rf_fit_tune <- 
  pima_rf_tune_wflow %>% 
  tune_grid(
    resamples = pima_treino_cv,
    grid = pima_rf_grid
  )

# resultados

collect_metrics(pima_rf_fit_tune)

pima_rf_fit_tune %>%
  collect_metrics() %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(., aes(x = mtry, y = mean, colour = min_n, group = min_n)) +
  geom_line() +
  geom_point() +
  facet_grid(~ .metric) +
  scale_x_continuous(breaks = seq(1, 9, 2)) +
  scale_colour_viridis_d()

# melhores modelos

pima_rf_fit_tune %>%
  show_best("roc_auc")

pima_rf_fit_tune %>%
  show_best("accuracy")

# melhor modelo

pima_rf_best <- 
  pima_rf_fit_tune %>%
  select_best("accuracy")

pima_rf_final <-
  pima_rf_tune_wflow %>%
  finalize_workflow(pima_rf_best)

pima_rf_final <- fit(pima_rf_final, 
                     pima_treino_t)

pima_rf_final

# 3

pima_rf_final %>% 
  extract_fit_parsnip() %>% 
  vip(scale = TRUE)

# 4

# resultados no conjunto de teste

resultado_rf <- 
  pima_teste_t %>%
  bind_cols(predict(pima_rf_final, pima_teste_t) %>%
            rename(predicao_rf = .pred_class))

metrics(resultado_rf, 
        truth = type, 
        estimate = predicao_rf,
        options = "roc")

conf_mat(resultado_rf, 
         truth = type, 
         estimate = predicao_rf) %>%
  autoplot(type = "heatmap")

sens(resultado_rf, 
     truth = type, 
     estimate = predicao_rf)

spec(resultado_rf, 
     truth = type, 
     estimate = predicao_rf)

# 5



