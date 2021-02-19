library(tidymodels)
theme_set(theme_bw())

ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() +
  labs(x = "Velocidade (mph)", y = "Distancia (pes)")

# determinacao do software

cars_lm <- 
  linear_reg() %>% 
  set_engine("lm")

# ajuste do modelo

cars_lm_fit <- 
  cars_lm %>% 
  fit(dist ~ speed, 
      data = cars)

# resultados

cars_lm_fit

# resultados

tidy(cars_lm_fit)

# semente aleatoria

set.seed(555)

# 75% dos dados como treino

cars_split <- initial_split(cars, prop = .75)
cars_split

# criar os conjuntos de dados de treino e teste

cars_treino <- training(cars_split)
nrow(cars_treino)/nrow(cars)

cars_teste  <- testing(cars_split)
nrow(cars_teste)/nrow(cars)

# receita

cars_rec <- 
  recipe(dist ~ speed, 
         data = cars_treino)

cars_rec

# modelo

cars_lm <- 
  linear_reg() %>% 
  set_engine("lm")

cars_lm

# criar workflow

cars_wflow <- 
  workflow() %>% 
  add_recipe(cars_rec) %>%
  add_model(cars_lm)

# ajuste do modelo

cars_lm_fit_treino <- fit(cars_wflow, cars_treino)

tidy(cars_lm_fit_treino)
tidy(cars_lm_fit)

# semente aleatoria

set.seed(321)

# divisao dos dados

cars_treino_cv <- vfold_cv(cars_treino, v = 5)

cars_treino_cv

# modelo ajustado com validacao cruzada

cars_lm_fit_cv <- fit_resamples(cars_wflow, cars_treino_cv)

cars_lm_fit_cv

# resultados

collect_metrics(cars_lm_fit_cv)

sqrt(mean((predict(cars_lm_fit$fit) - cars$dist)^2))

# resultados no conjunto de teste

resultado <- 
  cars_teste %>%
  bind_cols(predict(cars_lm_fit_treino, cars_teste) %>%
              rename(predicao_lm = .pred))

# resultado final

metrics(resultado, 
        truth = dist, 
        estimate = predicao_lm)

# grafico final

ggplot(resultado, aes(x = dist, y = predicao_lm)) +
  geom_point() +
  labs(x = "Valores Observados", y = "Valores Preditos") +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed()
