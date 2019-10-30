#config
    library(tidyverse)
    library(dslabs)
    data(heights)

#we can use the probability density to estimate the probability of someone being taller than 76 inches
x <- heights %>% filter(sex=="Male") %>% .$height
avg <- mean(x)
s <- sd(x)
1 - pnorm(76, avg, s)

#we get the probability density function in R using dnorm()
dnorm(76, avg, s)