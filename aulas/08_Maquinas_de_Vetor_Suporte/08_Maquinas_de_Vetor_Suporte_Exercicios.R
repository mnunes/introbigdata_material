# 1

# pacotes

library(tidymodels)
theme_set(theme_bw())
library(ggfortify)
library(GGally)

# dados

credito <- read.csv(file = "credito.csv")



# 2

####################
### treino/teste ###
####################

credito %>%
  group_by(Class) %>%
  count()

# semente aleatoria

set.seed(5659)

# 70% dos dados como treino

credito_split <- initial_split(credito, prop = .70, strata = Class)

# criar os conjuntos de dados de treino e teste

credito_treino <- training(credito_split)
nrow(credito_treino)/nrow(credito)

credito_teste  <- testing(credito_split)
nrow(credito_teste)/nrow(credito)

# eda

autoplot(prcomp(credito_treino %>% select(-Class), 
                center = TRUE, scale. = TRUE), 
         data = credito_treino,
         colour = "Class") +
  scale_colour_viridis_d()

# pre-processamento

credito_rec <- 
  recipe(Class ~ ., 
         data = credito_treino) %>%
  # remover observacoes de modo que todos os niveis de Class
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(Class) %>% 
  # center/scale
  step_center(-Class) %>% 
  step_scale(-Class) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

credito_treino_t <- juice(credito_rec)

# verificar o resultado do processamento de dados

credito_treino_t %>% 
  pivot_longer(-Class) %>% 
  ggplot(., aes(fill = Class)) +
  geom_histogram(aes(value)) +
  facet_wrap(~ name) +
  scale_fill_viridis_d()

# preparar o conjunto de teste

credito_teste_t <- bake(credito_rec, 
                        new_data = credito_teste)



# 3

##############
### tuning ###
##############

#########################################
# definicao do tuning para svm polinomial

credito_svm_poly_tune <-
  svm_poly(
    cost = tune(), 
    degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# grid de procura

credito_svm_poly_grid <- grid_regular(cost(range(-10, 5)),
                                      degree(),
                                      levels = c(10, 2))

credito_svm_poly_grid

# workflow

credito_svm_poly_tune_wflow <- 
  workflow() %>%
  add_model(credito_svm_poly_tune) %>%
  add_formula(Class ~ .)

# definicao da validacao cruzada

set.seed(6472)

credito_treino_cv <- vfold_cv(credito_treino_t, v = 10)

# avaliacao do modelo

credito_svm_poly_fit_tune <- 
  credito_svm_poly_tune_wflow %>% 
  tune_grid(
    resamples = credito_treino_cv,
    grid = credito_svm_poly_grid
  )

# resultados

collect_metrics(credito_svm_poly_fit_tune)

credito_svm_poly_fit_tune %>%
  collect_metrics() %>%
  mutate(degree = factor(degree)) %>%
  ggplot(., aes(x = cost, y = mean, colour = degree)) +
  geom_line() +
  geom_point() +
  facet_grid(~ .metric) +
  scale_x_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_viridis_d()

# melhores modelos

credito_svm_poly_fit_tune %>%
  show_best("roc_auc")

credito_svm_poly_fit_tune %>%
  show_best("accuracy")

# melhor modelo

credito_svm_poly_best <- 
  credito_svm_poly_fit_tune %>%
  select_best("accuracy")

credito_svm_poly_final <-
  credito_svm_poly_tune_wflow %>%
  finalize_workflow(credito_svm_poly_best)

credito_svm_poly_final <- fit(credito_svm_poly_final, credito_treino_t)

credito_svm_poly_final

# resultados no conjunto de teste

resultado_poly <- 
  credito_teste_t %>%
  bind_cols(predict(credito_svm_poly_final, credito_teste_t) %>%
              rename(predicao_svm_poly = .pred_class))

metrics(resultado_poly, 
        truth = Class, 
        estimate = predicao_svm_poly,
        options = "roc")


#####################################
# definicao do tuning para svm radial

credito_svm_rbf_tune <-
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# grid de procura

credito_svm_rbf_grid <- grid_regular(cost(range(-10, 5)),
                                     rbf_sigma(),
                                     levels = c(10, 5))

credito_svm_rbf_grid

# workflow

credito_svm_rbf_tune_wflow <- 
  workflow() %>%
  add_model(credito_svm_rbf_tune) %>%
  add_formula(Class ~ .)

# avaliacao do modelo

credito_svm_rbf_fit_tune <- 
  credito_svm_rbf_tune_wflow %>% 
  tune_grid(
    resamples = credito_treino_cv,
    grid = credito_svm_rbf_grid
  )

# resultados

collect_metrics(credito_svm_rbf_fit_tune)

credito_svm_rbf_fit_tune %>%
  collect_metrics() %>%
  mutate(rbf_sigma = factor(rbf_sigma)) %>%
  ggplot(., aes(x = cost, y = mean, colour = rbf_sigma)) +
  geom_line() +
  geom_point() +
  facet_grid(~ .metric) +
  scale_x_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_viridis_d()

# melhores modelos

credito_svm_rbf_fit_tune %>%
  show_best("roc_auc")

credito_svm_rbf_fit_tune %>%
  show_best("accuracy")

# melhor modelo

credito_svm_rbf_best <- 
  credito_svm_rbf_fit_tune %>%
  select_best("accuracy")

credito_svm_rbf_final <-
  credito_svm_rbf_tune_wflow %>%
  finalize_workflow(credito_svm_rbf_best)

credito_svm_rbf_final <- fit(credito_svm_rbf_final, credito_treino_t)

credito_svm_rbf_final

# resultados no conjunto de teste

resultado_rbf <- 
  credito_teste_t %>%
  bind_cols(predict(credito_svm_rbf_final, credito_teste_t) %>%
              rename(predicao_svm_rbf = .pred_class))

metrics(resultado_rbf, 
        truth = Class, 
        estimate = predicao_svm_rbf,
        options = "roc")

# comparacao polinomial e radial

conf_mat(resultado_poly, 
         truth = Class, 
         estimate = predicao_svm_poly)

conf_mat(resultado_poly, 
         truth = Class, 
         estimate = predicao_svm_poly) %>%
  autoplot(type = "heatmap") +
  scale_colour_viridis_c()

sens(resultado_poly, 
     truth = Class, 
     estimate = predicao_svm_poly)

spec(resultado_poly, 
     truth = Class, 
     estimate = predicao_svm_poly)

conf_mat(resultado_rbf, 
         truth = Class, 
         estimate = predicao_svm_rbf)

conf_mat(resultado_rbf, 
         truth = Class, 
         estimate = predicao_svm_rbf) %>%
  autoplot(type = "heatmap")

sens(resultado_rbf, 
     truth = Class, 
     estimate = predicao_svm_rbf)

spec(resultado_rbf, 
     truth = Class, 
     estimate = predicao_svm_rbf)

# 4

# pacotes

library(tidymodels)
theme_set(theme_bw())
library(GGally)

data(Sacramento, package = "caret")

residencias <- 
  Sacramento %>%
  select(price, type, sqft, baths, beds)

# 5

ggplot(residencias, aes(x = type, y = price)) +
  geom_boxplot()

# 6

residencias %>%
  select_if(is.numeric) %>%
  ggpairs()

# 7

# semente aleatoria

set.seed(3105)

# 70% dos dados como treino

res_split <- initial_split(residencias, prop = .75)

# criar os conjuntos de dados de treino e teste

res_treino <- training(res_split)
nrow(res_treino)/nrow(residencias)

res_teste  <- testing(res_split)
nrow(res_teste)/nrow(residencias)

# pre-processamento

res_rec <- 
  recipe(price ~ ., 
         data = res_treino) %>%
  # transformacao logaritmica
  step_log(sqft) %>%
  # categorica para numerica
  step_dummy(type) %>%
  # center/scale
  step_center(-price) %>% 
  step_scale(-price) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

res_treino_t <- juice(res_rec)

# verificar o resultado do processamento de dados

ggpairs(res_treino_t)

# preparar o conjunto de teste

res_teste_t <- bake(res_rec,
                    new_data = res_teste)



##############
### tuning ###
##############

#########################################
# definicao do tuning para svm polinomial

res_svm_poly_tune <-
  svm_poly(
    cost = tune(), 
    degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# grid de procura

res_svm_poly_grid <- grid_regular(cost(range(-10, 10)),
                                  degree(),
                                  levels = c(10, 2))

res_svm_poly_grid

# workflow

res_svm_poly_tune_wflow <- 
  workflow() %>%
  add_model(res_svm_poly_tune) %>%
  add_formula(price ~ .)

# definicao da validacao cruzada

set.seed(8273)

res_treino_cv <- vfold_cv(res_treino_t, v = 5)

# avaliacao do modelo

res_svm_poly_fit_tune <- 
  res_svm_poly_tune_wflow %>% 
  tune_grid(
    resamples = res_treino_cv,
    grid = res_svm_poly_grid
  )

# resultados

collect_metrics(res_svm_poly_fit_tune)

res_svm_poly_fit_tune %>%
  collect_metrics() %>%
  mutate(degree = factor(degree)) %>%
  ggplot(., aes(x = cost, y = mean, colour = degree)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free") +
  scale_x_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_viridis_d()

# melhores modelos

res_svm_poly_fit_tune %>%
  show_best("rmse")

res_svm_poly_fit_tune %>%
  show_best("rsq")

# melhor modelo

res_svm_poly_best <- 
  res_svm_poly_fit_tune %>%
  select_best("rmse")

res_svm_poly_final <-
  res_svm_poly_tune_wflow %>%
  finalize_workflow(res_svm_poly_best)

res_svm_poly_final <- fit(res_svm_poly_final, 
                          res_treino_t)

res_svm_poly_final

# resultados no conjunto de teste

resultado_poly <- 
  res_teste_t %>%
  bind_cols(predict(res_svm_poly_final, res_teste_t) %>%
              rename(predicao_svm_poly = .pred))

metrics(resultado_poly, 
        truth = price, 
        estimate = predicao_svm_poly)



#####################################
# definicao do tuning para svm radial

res_svm_rbf_tune <-
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# grid de procura

res_svm_rbf_grid <- grid_regular(cost(range(-10, 10)),
                                 rbf_sigma(),
                                 levels = c(10, 5))

res_svm_rbf_grid

# workflow

res_svm_rbf_tune_wflow <- 
  workflow() %>%
  add_model(res_svm_rbf_tune) %>%
  add_formula(price ~ .)

# avaliacao do modelo

res_svm_rbf_fit_tune <- 
  res_svm_rbf_tune_wflow %>% 
  tune_grid(
    resamples = res_treino_cv,
    grid = res_svm_rbf_grid
  )

# resultados

collect_metrics(res_svm_rbf_fit_tune)

res_svm_rbf_fit_tune %>%
  collect_metrics() %>%
  mutate(rbf_sigma = factor(rbf_sigma)) %>%
  ggplot(., aes(x = cost, y = mean, colour = rbf_sigma)) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free") +
  scale_x_continuous(trans = "log2") +
  scale_colour_viridis_d()

# melhores modelos

res_svm_rbf_fit_tune %>%
  show_best("rmse")

res_svm_rbf_fit_tune %>%
  show_best("rsq")

# melhor modelo

res_svm_rbf_best <- 
  res_svm_rbf_fit_tune %>%
  select_best("rmse")

res_svm_rbf_final <-
  res_svm_rbf_tune_wflow %>%
  finalize_workflow(res_svm_rbf_best)

res_svm_rbf_final <- fit(res_svm_rbf_final, 
                         res_treino_t)

res_svm_rbf_final

# resultados no conjunto de teste

resultado_rbf <- 
  res_teste_t %>%
  bind_cols(predict(res_svm_rbf_final, res_teste_t) %>%
              rename(predicao_svm_rbf = .pred))

metrics(resultado_rbf, 
        truth = price, 
        estimate = predicao_svm_rbf)



# 9

ggplot(resultado_poly, aes(x = price, y = predicao_svm_poly)) +
  geom_point() +
  geom_abline(linetype = "dashed")

ggplot(resultado_rbf, aes(x = price, y = predicao_svm_rbf)) +
  geom_point() +
  geom_abline(linetype = "dashed")

# kernel radial eh um pouco melhor


