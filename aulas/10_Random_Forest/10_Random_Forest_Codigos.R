# pacotes carregados

library(tidymodels)
theme_set(theme_bw())
library(onehot)
library(palmerpenguins)
library(GGally)
library(ggfortify)
library(vip)

# checagem dos dados

glimpse(penguins)

# criacao de variaveis dummy

pp <- 
  penguins %>%
  select(!where(is.numeric)) %>%
  select(-species) %>%
  onehot() %>%
  predict(penguins) %>%
  as.data.frame() %>%
  select(i_Biscoe    = `island=Biscoe`, 
         i_Dream     = `island=Dream`,
         i_Torgersen = `island=Torgersen`,
         s_fem       = `sex=female`,
         s_male      = `sex=male`)

pp <- 
  penguins %>%
  select(where(is.numeric), species, -year) %>%
  bind_cols(pp) %>%
  relocate(species) %>%
  na.omit()

# treino/teste

penguins %>%
  group_by(species) %>%
  count()

# 75% dos dados como treino

set.seed(1232)

pp_split <- initial_split(pp, prop = .75, strata = species)

# criar os conjuntos de dados de treino e teste

pp_treino <- training(pp_split)
pp_teste  <- testing(pp_split)

# eda

autoplot(prcomp(pp_treino %>% select(-species),
                center = TRUE, scale. = TRUE), 
         data = pp_treino,
         colour = "species") +
  scale_colour_viridis_d()

ggpairs(pp_treino %>% select(species, 
                             bill_length_mm,
                             bill_depth_mm,
                             flipper_length_mm,
                             body_mass_g), 
        aes(colour = species)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

# pre-processamento

pp_rec <- 
  recipe(species ~ ., 
         data = pp_treino) %>%
  # remover observacoes de modo que todos os niveis de species
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(species) %>% 
  # center/scale
  step_center(-species) %>% 
  step_scale(-species) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

# aplicar a transformacao aos dados

pp_treino_t <- juice(pp_rec)

# preparar o conjunto de teste

pp_teste_t <- bake(pp_rec,
                   new_data = pp_teste)



##############
### tuning ###
##############

#####################
# definicao do tuning

pp_rf_tune <-
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# grid de procura

pp_rf_grid <- grid_regular(mtry(range(1, 9)),
                           min_n(range(10, 50)),
                           levels = c(9, 5))

pp_rf_grid

# workflow

pp_rf_tune_wflow <- 
  workflow() %>%
  add_model(pp_rf_tune) %>%
  add_formula(species ~ .)

# definicao da validacao cruzada

set.seed(2389)

pp_treino_cv <- vfold_cv(pp_treino_t, v = 7)

# avaliacao do modelo

pp_rf_fit_tune <- 
  pp_rf_tune_wflow %>% 
  tune_grid(
    resamples = pp_treino_cv,
    grid = pp_rf_grid
  )

# resultados

collect_metrics(pp_rf_fit_tune)

pp_rf_fit_tune %>%
  collect_metrics() %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(., aes(x = mtry, y = mean, colour = min_n, group = min_n)) +
  geom_line() +
  geom_point() +
  facet_grid(~ .metric) +
  scale_x_continuous(breaks = seq(1, 9, 2)) +
  scale_colour_viridis_d()

# melhores modelos

pp_rf_fit_tune %>%
  show_best("roc_auc")

pp_rf_fit_tune %>%
  show_best("accuracy")

# melhor modelo

pp_rf_best <- 
  pp_rf_fit_tune %>%
  select_best("accuracy")

pp_rf_final <-
  pp_rf_tune_wflow %>%
  finalize_workflow(pp_rf_best)

pp_rf_final <- fit(pp_rf_final, 
                   pp_treino_t)

pp_rf_final

# resultados no conjunto de teste

resultado_rf <- 
  pp_teste_t %>%
  bind_cols(predict(pp_rf_final, pp_teste_t) %>%
              rename(predicao_rf = .pred_class))

metrics(resultado_rf, 
        truth = species, 
        estimate = predicao_rf,
        options = "roc")

conf_mat(resultado_rf, 
         truth = species, 
         estimate = predicao_rf) %>%
  autoplot(type = "heatmap")

# sensitividade

sens(resultado_rf, 
     truth = species, 
     estimate = predicao_rf)

# especificidade

spec(resultado_rf, 
     truth = species, 
     estimate = predicao_rf)

# importancia das variaveis

pp_rf_final %>% 
  pull_workflow_fit() %>% 
  vip(scale = TRUE)



