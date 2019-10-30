#config
library(tidyverse)
library(dslabs)
data(heights)

#define x as the heights of males
x <- heights %>% filter(sex == "Male") %>% .$height

#we can now generate data that looks like our reported heights
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n,avg,s)

#now we can plot our simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
    ggplot(aes(simulated_heights)) +
    geom_histogram(color = "black", binwidth = 2)

#with our simulated data we can nwow run a Monte Carlo to find the likelihood that someone is 7 foot
B <- 10000
tallest <- replicate(B,{
    simulated_data <- rnorm(800,avg,s)
    max(simulated_heights)
})

mean(tallest >= 7*12)