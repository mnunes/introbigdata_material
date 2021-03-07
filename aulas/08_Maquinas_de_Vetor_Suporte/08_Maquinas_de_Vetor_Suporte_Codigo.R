##############
### ajuste ###
##############

# pacotes

library(tidymodels)
theme_set(theme_bw())
library(ggfortify)
library(onehot)
library(janitor)

# dados

data("Ionosphere", package = "mlbench")

# criacao de variaveis dummy

ionosphere <- 
  Ionosphere %>%
  select(!where(is.numeric)) %>%
  select(-V2, -Class) %>%
  onehot() %>%
  predict(Ionosphere) %>%
  as.data.frame() %>%
  select(V1_0 = `V1=0`, V1_1 = `V1=1`)

ionosphere <- 
  Ionosphere %>%
  select(-V1, -V2) %>%
  bind_cols(ionosphere) %>%
  relocate(Class)



####################
### treino/teste ###
####################

ionosphere %>%
  group_by(Class) %>%
  count()

# semente aleatoria

set.seed(5659)

# 70% dos dados como treino

ionosphere_split <- initial_split(ionosphere, prop = .70, strata = Class)

# criar os conjuntos de dados de treino e teste

ionosphere_treino <- training(ionosphere_split)
nrow(ionosphere_treino)/nrow(ionosphere)

ionosphere_teste  <- testing(ionosphere_split)
nrow(ionosphere_teste)/nrow(ionosphere)

# eda

autoplot(prcomp(ionosphere_treino %>% select(-Class), 
                center = TRUE, scale. = TRUE), 
         data = ionosphere_treino,
         colour = "Class") +
  scale_colour_viridis_d()

ionosphere_treino %>%
  pivot_longer(-Class) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ name)

# pre-processamento

ionosphere_rec <- 
  recipe(Class ~ ., 
         data = ionosphere_treino) %>%
  # remover observacoes de modo que todos os niveis de Class
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(Class) %>% 
  # center/scale
  step_center(-Class) %>% 
  step_scale(-Class) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

ionosphere_treino_t <- juice(ionosphere_rec)

# verificar o resultado do processamento de dados

ionosphere_treino_t %>% 
  pivot_longer(-Class) %>% 
  ggplot(., aes(fill = Class)) +
  geom_histogram(aes(value)) +
  facet_wrap(~ name) +
  scale_fill_viridis_d()

# preparar o conjunto de teste

ionosphere_teste_t <- bake(ionosphere_rec,
                           new_data = ionosphere_teste)



##############
### tuning ###
##############

#########################################
# definicao do tuning para svm polinomial

ionosphere_svm_poly_tune <-
  svm_poly(
    cost = tune(), 
    degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# grid de procura

ionosphere_svm_poly_grid <- grid_regular(cost(range(-10, 5)),
                                   degree(),
                                   levels = c(10, 2))

ionosphere_svm_poly_grid

# workflow

ionosphere_svm_poly_tune_wflow <- 
  workflow() %>%
  add_model(ionosphere_svm_poly_tune) %>%
  add_formula(Class ~ .)

# definicao da validacao cruzada

set.seed(6472)

ionosphere_treino_cv <- vfold_cv(ionosphere_treino_t, v = 10)

# avaliacao do modelo

ionosphere_svm_poly_fit_tune <- 
  ionosphere_svm_poly_tune_wflow %>% 
  tune_grid(
    resamples = ionosphere_treino_cv,
    grid = ionosphere_svm_poly_grid
  )

# resultados

collect_metrics(ionosphere_svm_poly_fit_tune)

ionosphere_svm_poly_fit_tune %>%
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

ionosphere_svm_poly_fit_tune %>%
  show_best("roc_auc")

ionosphere_svm_poly_fit_tune %>%
  show_best("accuracy")

# melhor modelo

ionosphere_svm_poly_best <- 
  ionosphere_svm_poly_fit_tune %>%
  select_best("accuracy")

ionosphere_svm_poly_final <-
  ionosphere_svm_poly_tune_wflow %>%
  finalize_workflow(ionosphere_svm_poly_best)

ionosphere_svm_poly_final <- fit(ionosphere_svm_poly_final, 
                                 ionosphere_treino_t)

ionosphere_svm_poly_final

# resultados no conjunto de teste

resultado_poly <- 
  ionosphere_teste_t %>%
  bind_cols(predict(ionosphere_svm_poly_final, ionosphere_teste_t) %>%
              rename(predicao_svm_poly = .pred_class))

metrics(resultado_poly, 
        truth = Class, 
        estimate = predicao_svm_poly,
        options = "roc")


#####################################
# definicao do tuning para svm radial

ionosphere_svm_rbf_tune <-
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# grid de procura

ionosphere_svm_rbf_grid <- grid_regular(cost(range(-10, 5)),
                                        rbf_sigma(),
                                        levels = c(10, 5))

ionosphere_svm_rbf_grid

# workflow

ionosphere_svm_rbf_tune_wflow <- 
  workflow() %>%
  add_model(ionosphere_svm_rbf_tune) %>%
  add_formula(Class ~ .)

# avaliacao do modelo

ionosphere_svm_rbf_fit_tune <- 
  ionosphere_svm_rbf_tune_wflow %>% 
  tune_grid(
    resamples = ionosphere_treino_cv,
    grid = ionosphere_svm_rbf_grid
  )

# resultados

collect_metrics(ionosphere_svm_rbf_fit_tune)

ionosphere_svm_rbf_fit_tune %>%
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

ionosphere_svm_rbf_fit_tune %>%
  show_best("roc_auc")

ionosphere_svm_rbf_fit_tune %>%
  show_best("accuracy")

# melhor modelo

ionosphere_svm_rbf_best <- 
  ionosphere_svm_rbf_fit_tune %>%
  select_best("accuracy")

ionosphere_svm_rbf_final <-
  ionosphere_svm_rbf_tune_wflow %>%
  finalize_workflow(ionosphere_svm_rbf_best)

ionosphere_svm_rbf_final <- fit(ionosphere_svm_rbf_final, 
                                ionosphere_treino_t)

ionosphere_svm_rbf_final

# resultados no conjunto de teste

resultado_rbf <- 
  ionosphere_teste_t %>%
  bind_cols(predict(ionosphere_svm_rbf_final, ionosphere_teste_t) %>%
              rename(predicao_svm_rbf = .pred_class))

metrics(resultado_rbf, 
        truth = Class, 
        estimate = predicao_svm_rbf,
        options = "roc")

# comparacao polinomial e radial

# comparacao polinomial e radial

conf_mat(resultado_poly, 
         truth = Class, 
         estimate = predicao_svm_poly)

conf_mat(resultado_poly, 
         truth = Class, 
         estimate = predicao_svm_poly) %>%
  autoplot(type = "heatmap")

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

