#config
library(tidyverse)
library(dslabs)
data(heights)
options(digits = 3)

#for an empirical cumulative distribution function (eCDF) first we define a vector 
x <- heights %>% filter(sex == "Male") %>% .$height

#now we can define the function that will return the proportion of a smaller or equal to the mean of x
F <- function(a) mean(x<=a)
1- F(70)

#now that we have this defined we can also find the proportion of any subset
F(70)-F(68)