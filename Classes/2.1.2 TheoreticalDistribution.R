#config
library(tidyverse)
library(dslabs)
data(heights)
options(digits = 3)

#a distribution is normal if F(a) = pnorm(a,avg,s)
x <- heights %>% filter(sex == "Male") %>% .$height
F <- function(a) mean(x<=a)

#this also means we dont need the whole dataset to find proportions, just the mean and sd
1-pnorm(70.5,mean(x),sd(x)) #normal approximaation
1-F(70.5) #actual results

#although we dont as aspiring data scientists we use mainly discrete variables
#but we need to know the method for continuous variables so we force ourselves to change
#each height could be its own category and we can plot out the distribution by proportion of unique heights
plot(prop.table(table(x)), xlab = "height in inches",ylab = "PR(X=a)")

#continuous distribution is particuarly good at deeling with intervals of exactly one round number
#this uses the data not an approximation
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

#this uses the approximated normal distribution
pnorm(68.5,mean(x),sd(x)) - pnorm(67.5,mean(x),sd(x))
pnorm(69.5,mean(x),sd(x)) - pnorm(68.5,mean(x),sd(x))
pnorm(70.5,mean(x),sd(x)) - pnorm(69.5,mean(x),sd(x))

#there are times where the approximations are not useful
#such as when there isnt an integer included
#this is discretisation, where rounded numbers are skewing the data
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9,mean(x),sd(x)) - pnorm(70.1,mean(x),sd(x))