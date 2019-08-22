## Slides and code for the _Applied Machine Learning_ short course at the  
## 2019 R/Pharma conference.

## Part 1 ----------------------------------------------------------------------

library(tidymodels)

## -----------------------------------------------------------------------------

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

library(readr)

ames_prices <- "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )

## -----------------------------------------------------------------------------

# purrr loaded with tidyverse or tidymodels package

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))

head(mini_ames, n = 5)

## -----------------------------------------------------------------------------

by_alley <- split(mini_ames, mini_ames$Alley)
# map(.x, .f, ...)
map(by_alley, head, n = 2)

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

## -----------------------------------------------------------------------------

ames_lst_col <- nest(mini_ames, -Alley)
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~max(.x$Sale_Price))
  )

## -----------------------------------------------------------------------------

library(AmesHousing)
ames <- make_ames()

## Part 2 ----------------------------------------------------------------------

ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

## -----------------------------------------------------------------------------

# rsample functions
# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)
## -----------------------------------------------------------------------------

# result of initial_split()
# <training / testing / total>
data_split

## -----------------------------------------------------------------------------

training(data_split)

## -----------------------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

## -----------------------------------------------------------------------------

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

## -----------------------------------------------------------------------------

spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- set_engine(spec_lin_reg, "lm")
spec_lm

## -----------------------------------------------------------------------------

fit_lm <- fit(
  spec_lm,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

fit_lm

## -----------------------------------------------------------------------------

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  spec_lm,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log %>% dplyr::select(Latitude, Longitude)
)

## -----------------------------------------------------------------------------

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

## -----------------------------------------------------------------------------

coef(fit_stan$fit)

coef(fit_lm$fit)

## -----------------------------------------------------------------------------

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
fit_knn

## -----------------------------------------------------------------------------

# Numeric predictions always in a df
# with column `.pred`
test_pred <- fit_lm %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  slice(1:3)

## -----------------------------------------------------------------------------

# yardstick loaded by tidymodels

perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)

## Part 3 ----------------------------------------------------------------------

ggplot(ames_train, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")

## -----------------------------------------------------------------------------

# recipes loaded by tidymodels
mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

## -----------------------------------------------------------------------------

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec

## -----------------------------------------------------------------------------

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

mod_rec_trained

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)

## ----rec-data-args, eval = FALSE-----------------------------------------
## recipe(..., data = data_set)
## prep(...,   training = data_set)
## bake(...,   new_data = data_set)

## -----------------------------------------------------------------------------

ggplot(
  ames_train, 
  aes(x = Year_Built, y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  geom_smooth(method = "loess")

library(MASS) # to get robust linear regression model

ggplot(
  ames_train, 
  aes(x = Year_Built, 
      y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 

## -----------------------------------------------------------------------------

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)

## -----------------------------------------------------------------------------

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

## -----------------------------------------------------------------------------

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

## -----------------------------------------------------------------------------

ames_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

## -----------------------------------------------------------------------------

ames_rec <- prep(ames_rec)

fit_lm <- 
  spec_lm %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec))   # The recipe puts Sale_Price on the log scale

glance(fit_lm$fit)

holdout_data <- bake(ames_rec, ames_test, all_predictors())

## Part 4 ----------------------------------------------------------------------

set.seed(2453)
cv_splits <- vfold_cv(ames_train) #10-fold is default
cv_splits

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

## -----------------------------------------------------------------------------

cv_splits <- 
  cv_splits %>% 
  mutate(recipes = map(splits, prepper, recipe = ames_rec, fresh = TRUE))
cv_splits

map_dfr(cv_splits$recipes, tidy, number = 2) %>% 
  dplyr::filter(terms == "Lot_Area") %>% 
  slice(1:6)

## -----------------------------------------------------------------------------

parsnip_fit <- function(rec_obj, model) {
  fit(model, Sale_Price ~ ., data = juice(rec_obj))
}

knn_mod <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn")

cv_splits <- 
  cv_splits %>% 
  mutate(knn = map(recipes, parsnip_fit, model = knn_mod))

cv_splits %>% slice(1:6)

## -----------------------------------------------------------------------------

cv_splits %>% 
  dplyr::select(splits, recipes, knn) %>% 
  slice(1:4)

parsnip_metrics <- function(split, recipe, model) {
  raw_assessment <- assessment(split)
  processed <- bake(recipe, new_data = raw_assessment)
  
  model %>%
    predict(new_data = processed) %>%
    # Add the baked assessment data back in
    bind_cols(processed) %>% 
    perf_metrics(Sale_Price, .pred)
}

## -----------------------------------------------------------------------------

cv_splits <- cv_splits %>%
  mutate(
    metrics = pmap(
      list(
        split  = splits, 
        recipe = recipes, 
        model  = knn
      ),
      parsnip_metrics 
    )
  )

cv_splits %>% 
  dplyr::select(splits, recipes, knn, metrics) %>% 
  slice(1:4)

cv_splits$metrics[[1]]

## -----------------------------------------------------------------------------

cv_splits %>%
  unnest(metrics) %>%
  group_by(.metric) %>%
  summarise(
    mean = mean(.estimate),
    std_err = sd(.estimate)/sqrt(sum(!is.na(.estimate)))
  )

## -----------------------------------------------------------------------------

one_to_five <- 
  multi_predict(
    cv_splits$knn[[1]],
    new_data = juice(cv_splits$recipes[[1]]) %>% slice(1:2),
    neighbors = 1:5
  )
one_to_five

one_to_five %>% 
  add_rowindex() %>% 
  unnest()

## -----------------------------------------------------------------------------

more_metrics <- function(split, recipe, fit) {
  holdout_dat <- bake(recipe, new_data = assessment(split))
  
  resample_name <- labels(split)$id
  
  multi_predict(fit,
                new_data = holdout_dat,
                neighbors = 1:50) %>%
    bind_cols(dplyr::select(holdout_dat, Sale_Price)) %>%
    unnest() %>% 
    group_by(neighbors) %>% 
    perf_metrics(Sale_Price, .pred) %>%
    mutate(resample = resample_name)
}

cv_splits <- 
  cv_splits %>% 
  mutate(
    tuning_metrics = 
      pmap(list(splits, recipes, knn), more_metrics)
  )

cv_splits %>% 
  dplyr::select(tuning_metrics)

## -----------------------------------------------------------------------------

rs_rmse <- 
  cv_splits %>% 
  unnest(tuning_metrics) %>% 
  dplyr::filter(.metric == "rmse") %>% 
  group_by(neighbors) %>% 
  dplyr::summarize(rmse = mean(.estimate, na.rm = TRUE)) 

rs_rmse %>% slice(1:3)

rs_rmse %>% arrange(rmse) %>% slice(1)

ggplot(rs_rmse, aes(x = neighbors,y = rmse)) + 
  geom_point() + 
  geom_path()

## -----------------------------------------------------------------------------

# The entire training set is used for the recipe and model
ames_rec_final <- prep(ames_rec)

knn_mod_final <- 
  update(knn_mod, neighbors = 11) %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec_final))


