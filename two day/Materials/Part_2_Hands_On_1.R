# Slides for Applied Machine Learning workshop 

# Part_2_Basic_Principles - Hands-On: Some Basic Diagnostics #1

# ----------------------------------------------------------------

library(tidymodels)

# ----------------------------------------------------------------

library(AmesHousing)
ames <-
  make_ames() %>%
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)

# ----------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
simple_lm_values <- augment(simple_lm)

# ----------------------------------------------------------------

# From these results, let's take 10 minutes and do some visualizations:
#
#  - Plot the observed versus fitted values
#
#  - Plot the residuals
#
#  - Plot the predicted versus residuals
#
# Are there any downsides to this approach?

ggplot(simple_lm_values, aes(x = log10.Sale_Price., y = .fitted)) + 
  geom_point(alpha = 0.5, cex = .5) +
  geom_abline() 

simple_lm_values %>% 
  mutate(
    lon_cut = cut(Longitude, breaks = 5),
    lat_cut = cut(Latitude, breaks = 5)
  ) %>% 
  ggplot(aes(x = log10.Sale_Price., y = .fitted)) + 
  geom_point(alpha = 0.5, cex = .5) +
  geom_abline() + 
  facet_grid(lon_cut ~ lat_cut)


ggplot(simple_lm_values, aes(x = Longitude, y = .resid)) + 
  geom_point(alpha = 0.5, cex = .5) + 
  geom_smooth()

ggplot(simple_lm_values, aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.5, cex = .5) + 
  geom_smooth()




