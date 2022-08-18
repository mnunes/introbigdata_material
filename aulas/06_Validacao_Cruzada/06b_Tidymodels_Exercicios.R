library(tidymodels)
theme_set(theme_bw())

#########
### 1 ###
#########

mpg2 <-
  mpg %>%
  select_if(is.numeric)

head(mpg2)


#########
### 2 ###
#########

mpg2 %>%
  cor() %>%
  as.data.frame() %>%
  select(hwy) %>%
  arrange(desc(hwy))

ggplot(mpg2, aes(x = displ, y = hwy)) + 
  geom_point() +
  labs(x = "Consumo na Cidade", y = "Consumo na Estrada")



#########
### 3 ###
#########

# semente aleatoria

set.seed(555)

# 80% dos dados como treino

mpg2_split <- initial_split(mpg2, prop = .80)
mpg2_split

# criar os conjuntos de dados de treino e teste

mpg2_treino <- training(mpg2_split)
nrow(mpg2_treino)/nrow(mpg2)

mpg2_teste  <- testing(mpg2_split)
nrow(mpg2_teste)/nrow(mpg2)



#########
### 4 ###
#########

# modelo

mpg2_lm <- 
  linear_reg() %>% 
  set_engine("lm")

# receita

mpg2_rec <- 
  recipe(hwy ~ displ, 
         data = mpg2_treino)

# criar workflow

mpg2_wflow <- 
  workflow() %>% 
  add_recipe(mpg2_rec) %>%
  add_model(mpg2_lm)

# validacao cruzada

set.seed(123)

mpg2_treino_cv <- vfold_cv(mpg2_treino, v = 5)

mpg2_treino_cv

# modelo ajustado com validacao cruzada

mpg2_lm_fit_cv <- fit_resamples(mpg2_wflow, mpg2_treino_cv)

mpg2_lm_fit_cv



#########
### 5 ###
#########

# resultados treino

collect_metrics(mpg2_lm_fit_cv)

# resultados teste

mpg2_lm_fit_treino <- fit(mpg2_wflow, mpg2_treino)

resultado <- 
  mpg2_teste %>%
  bind_cols(predict(mpg2_lm_fit_treino, mpg2_teste) %>%
              rename(predicao_lm = .pred))

metrics(resultado, 
        truth = hwy, 
        estimate = predicao_lm)



#########
### 6 ###
#########

# receita

mpg2_rec_multipla <- 
  recipe(hwy ~ ., 
         data = mpg2_treino) %>%
  # center/scale
  step_center(-hwy) %>% 
  step_scale(-hwy) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()

mpg2_treino_t <- juice(mpg2_rec_multipla)

head(mpg2_treino_t)

# criar workflow

mpg2_wflow_multipla <- 
  workflow() %>% 
  add_recipe(mpg2_rec_multipla) %>%
  add_model(mpg2_lm)

mpg2_wflow_multipla

# modelo ajustado com validacao cruzada

set.seed(123)

mpg2_treino_t_cv <- vfold_cv(mpg2_treino_t, v = 5)

mpg2_lm_fit_cv_multipla <- fit_resamples(mpg2_wflow_multipla, mpg2_treino_cv)



#########
### 7 ###
#########

collect_metrics(mpg2_lm_fit_cv_multipla)

# resultados no conjunto de teste

mpg2_teste_t <- bake(mpg2_rec_multipla,
                     new_data = mpg2_teste)

mpg2_lm_fit_treino_multipla <- fit(mpg2_wflow_multipla, mpg2_treino_t)

resultado_multipla <- 
  mpg2_teste_t %>%
  bind_cols(predict(mpg2_lm_fit_treino_multipla, mpg2_teste_t) %>%
              rename(predicao_lm = .pred))

metrics(resultado_multipla, 
        truth = hwy, 
        estimate = predicao_lm)

ggplot(resultado_multipla, aes(x = hwy, y = predicao_lm)) +
  geom_point() +
  labs(x = "Valores Observados", y = "Valores Preditos") +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed()

# ambos os resultados

left_join(resultado, resultado_multipla, by = "hwy") %>%
  select(hwy, 
         Reg_Simples = predicao_lm.x, 
         Reg_Multipla = predicao_lm.y) %>%
  melt(id.var = "hwy") %>%
  ggplot(., aes(x = hwy, y = value)) +
  geom_point() +
  labs(x = "Valores Observados", y = "Valores Preditos") +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed() +
  facet_wrap(~ variable, nrow = 2)



