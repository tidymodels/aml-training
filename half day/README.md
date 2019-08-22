Slides and code for the _Applied Machine Learning_ short course at R/Pharma 2019.

If you would like to run teh computations on your local machine, please run this code to install and verify the packages:

```r
install.packages(
  c("tidymodels", "kknn", "AmesHousing", "readr"), 
  repos = "http://cran.r-project.org"
)

library(tidymodels)
library(AmesHousing)
library(readr)
library(kknn)
``` 

Warnings here are okay, errors are not. 

If you want to read up a bit about predictive modeling before the workshop, check out [chapter 1](https://bookdown.org/max/FES/intro-intro.html) and [chapter 3](https://bookdown.org/max/FES/review-predictive-modeling-process.html) of [_Feature Engineering and Selection_](https://bookdown.org/max/FES/). 
