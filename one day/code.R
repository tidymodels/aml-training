## Slides and code for the _Modeling in the Tidyverse_ short course on 
## Wednesday, May 29 2019 at SDSS (Symposium on Data Science and Statistics).

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
cv_splits <- vfold_cv(
  data = ames_train, 
  v = 10, 
  strata = "Sale_Price"
)
cv_splits %>% slice(1:6)

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

## -----------------------------------------------------------------------------

geo_form <- log10(Sale_Price) ~ Latitude + Longitude

# Fit on a single analysis resample
fit_model <- function(split, spec) {
  fit(
    object = spec, 
    formula = geo_form,
    data = analysis(split) # <- pull out training set
  )
}

# For each resample, call fit_model()
cv_splits <- cv_splits %>% 
  mutate(models_lm = map(splits, fit_model, spec_lm))

cv_splits

## -----------------------------------------------------------------------------

lm_pred <- function(split, model) {
  
  # Extract the assessment set
  assess <- assessment(split) %>%
    mutate(Sale_Price_Log = log10(Sale_Price))
  
  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)
  
  bind_cols(assess, pred)
}

## -----------------------------------------------------------------------------

cv_splits <- cv_splits %>%
  mutate(
    pred_lm = map2(splits, models_lm, lm_pred)
  )
cv_splits

## -----------------------------------------------------------------------------

lm_metrics <- function(pred_df) {
  # Use our previous yardstick function
  perf_metrics(
    pred_df, 
    truth = Sale_Price_Log, 
    estimate = .pred
  )
}

cv_splits <- cv_splits %>%
  mutate(perf_lm = map(pred_lm, lm_metrics))

dplyr::select(cv_splits, pred_lm, perf_lm)

cv_splits$perf_lm[[1]]

## -----------------------------------------------------------------------------

cv_splits %>%
  unnest(perf_lm) %>%
  group_by(.metric) %>%
  summarise(
    .avg = mean(.estimate),
    .sd = sd(.estimate)
  )

## -----------------------------------------------------------------------------

cv_splits <- 
  cv_splits %>% 
  # remove previous linear model stuff
  dplyr::select(-models_lm, -pred_lm, -perf_lm ) %>% 
  mutate(lm_rec = map(splits, prepper, recipe = ames_rec))
cv_splits %>% slice(1:4)

## -----------------------------------------------------------------------------

spec_lm <- linear_reg() %>%
  set_engine("lm")

parsnip_fit <- function(rec_obj, model) {
  fit(model, Sale_Price ~ ., data = juice(rec_obj))
}

cv_splits <- cv_splits %>% 
  mutate(
    lm = map(lm_rec, parsnip_fit, model = spec_lm)
  )

glance(cv_splits$lm[[1]]$fit)

## -----------------------------------------------------------------------------

cv_splits %>% 
  dplyr::select(splits, lm_rec, lm) %>% 
  slice(1:4)

## -----------------------------------------------------------------------------

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
    lm_res = pmap(
      list(
        split  = splits, 
        recipe = lm_rec, 
        model  = lm
      ),
      parsnip_metrics 
    )
  )

cv_splits %>% 
  dplyr::select(splits, lm_rec, lm, lm_res) %>% 
  slice(1:4)

## -----------------------------------------------------------------------------

cv_splits %>%
  unnest(lm_res) %>%
  group_by(.metric) %>%
  summarise(
    resampled_estimate = mean(.estimate)
  )

## -----------------------------------------------------------------------------

bst_mod <- 
  boost_tree(mode = "regression", trees = 50) %>% 
  set_engine("xgboost")

# No need for nonlinear terms, interactions, or "other groups"
bst_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_dummy(all_nominal()) 

parsnip_fit 

## -----------------------------------------------------------------------------

ex_fit <- 
  bst_mod %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, 
      data = ames_train)

smol_test <- 
  ames_test %>% 
  dplyr::select(Longitude, Latitude) %>% 
  slice(1:3)

ex_pred <- multi_predict(ex_fit, smol_test, trees = 20:21)

ex_pred

ex_pred$.pred[[1]]
unnest(ex_pred)

## -----------------------------------------------------------------------------

bst_metrics <- function(split, recipe, fit) {
  holdout_dat <- bake(recipe, new_data = assessment(split))
  
  pred_vars <- fit$fit$feature_names
  resample_name <- labels(split)$id
  
  multi_predict(fit,
                new_data = holdout_dat %>% 
                  dplyr::select(one_of(pred_vars)),
                trees = 1:50) %>%
    bind_cols(dplyr::select(holdout_dat, Sale_Price)) %>%
    unnest() %>% 
    group_by(trees) %>% 
    perf_metrics(Sale_Price, .pred) %>%
    mutate(resample = resample_name)
}

## -----------------------------------------------------------------------------

bst_splits <- 
  cv_splits %>% 
  dplyr::select(splits, id, lm_res) %>% 
  mutate(
    recipes = map(splits, prepper, recipe = bst_rec),
    boosting = map(recipes, parsnip_fit, model = bst_mod),
    boosting_res = pmap(list(splits, recipes, boosting), 
                        bst_metrics)
  )

bst_splits %>% dplyr::select(-splits, -lm_res)

## -----------------------------------------------------------------------------

rs_rmse <- 
  bst_splits %>% 
  unnest(boosting_res) %>% 
  dplyr::filter(.metric == "rmse") %>% 
  group_by(trees) %>% 
  dplyr::summarize(rmse = mean(.estimate, na.rm = TRUE)) 

rs_rmse %>% slice(1:3)

rs_rmse %>% arrange(rmse) %>% slice(1)

## -----------------------------------------------------------------------------

ggplot(rs_rmse, aes(x = trees,y = rmse)) + 
  geom_point() + 
  geom_path()

## -----------------------------------------------------------------------------

# The entire training set is used for the recipe and model
bst_rec_final <- prep(bst_rec)

bst_mod_final <- 
  update(bst_mod, trees = 20) %>% 
  fit(Sale_Price ~ ., data = juice(bst_rec_final))

## Part 5 ----------------------------------------------------------------------

bst_rmse <- 
  bst_splits %>% 
  unnest(boosting_res) %>% 
  dplyr::filter(.metric == "rmse") %>% 
  group_by(trees) %>% 
  dplyr::summarize(rmse = mean(.estimate, na.rm = TRUE)) %>% 
  arrange(rmse) %>% 
  slice(1) %>% 
  pull(rmse)

lm_rmse <- 
  bst_splits %>% 
  unnest(lm_res) %>% 
  filter(.metric == "rmse") %>% 
  pull(.estimate) %>% 
  mean()

## -----------------------------------------------------------------------------

rmse_results <-
  bst_splits %>%
  mutate(
    boosting =
      map_dbl(
        boosting_res, ~ .x %>% filter(trees == 20 & .metric == "rmse") %>% pull(.estimate)
      ),
    linear_reg = map_dbl(lm_res, ~ .x %>% filter(.metric == "rmse") %>% pull(.estimate))
  ) %>%
  dplyr::select(splits, id, boosting, linear_reg)

rmse_results %>% slice(1:5)

## -----------------------------------------------------------------------------

rmse_results %>% 
  dplyr::select(-splits) %>% 
  gather(model, rmse, -id) %>% 
  ggplot() + 
  aes(x = model, y = rmse, group = id, color = id) +
  geom_line(alpha = .8) + 
  theme(legend.position = "none")

## -----------------------------------------------------------------------------

rmse_results %>% 
  mutate(diffs = boosting - linear_reg) %>% 
  pull(diffs) %>% 
  t.test()

## -----------------------------------------------------------------------------

library(tidyposterior)
library(rstanarm) # needed for my prior
rmse_mod <- 
  perf_mod(rmse_results, 
           seed = 4344, iter = 5000, 
           prior_intercept = student_t(df = 1))

## -----------------------------------------------------------------------------

theme_set(thm)

posteriors <- tidy(rmse_mod, seed = 366784)
summary(posteriors)

ggplot(posteriors)

## -----------------------------------------------------------------------------


differences <- contrast_models(rmse_mod, seed = 2581)

ggplot(differences, size = 0.05)

summary(differences, size = 0.05)

