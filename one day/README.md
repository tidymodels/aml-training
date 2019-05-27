Slides and code for the _Modeling in the Tidyverse_ short course on Wednesday, May 29 2019 at SDSS (Symposium on Data Science and Statistics).

**This repo will be updated a few days before the workshop with the slides and code**

To prepare, please run this code to install and verify the packages:

```r
install.packages(
  c("tidymodels", "tidyposterior", "AmesHousing", "readr", "xgboost"), 
  repos = "http://cran.r-project.org"
)

library(tidymodels)
library(xgboost)
library(tidyposterior)
library(AmesHousing)
library(readr)
``` 

Warnings here are okay, errors are not. 

We will have Rstudio Server instances to use if you can't (or don't want to) install these packages. 

If you want to read up a bit about predictive modeling before the workshop, check out [chapter 1](https://bookdown.org/max/FES/intro-intro.html) and [chapter 3](https://bookdown.org/max/FES/review-predictive-modeling-process.html) of [_Feature Engineering and Selection_](https://bookdown.org/max/FES/). 