# Day 1 ----------------------------------------------------------


library(tidymodels)
theme_set(theme_bw())


library(AmesHousing)
ames <- make_ames()

ggplot(ames, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10() + 
  geom_smooth()


ggplot(ames, aes(x = factor(Fireplaces), y = Sale_Price)) + 
  geom_boxplot() + 
  scale_y_log10() 

ggplot(ames, aes(x = Sale_Condition, y = Sale_Price)) + 
  geom_boxplot() + 
  scale_y_log10() 


ggplot(ames, aes(x = Gr_Liv_Area)) + 
  geom_histogram()

ggplot(ames, aes(x = Gr_Liv_Area, y = First_Flr_SF)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10()  + 
  scale_x_log10() 

ggplot(holdout_results, aes(x = Neighborhood, y = .resid)) + 
  geom_boxplot() + coord_flip()


ggplot(ames, aes(x = Lot_Area, y = Sale_Price)) + 
  facet_wrap(~Neighborhood) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm) + 
  scale_y_log10()  + 
  scale_x_log10() 

cv_splits$models_lm[[1]] %>% pluck("fit") %>% broom::tidy()


# Day 2 ----------------------------------------------------------

