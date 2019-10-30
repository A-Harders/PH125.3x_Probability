#config
library(tidyverse)

#sampling models are ubiquitos in data science
#we are going to design a roulette wheel to see how a casino would analyse winnings or losing
wheel <- rep(c("red","black","green"),times = c(18,18,2))

#now we are going to run our game 1000 times
n <- 1000
X <- sample(ifelse(wheel=="red",-1,1),n,replace=TRUE)
X[1:10]

#as we know the proportions of wins and loses we can generate the draws on a single line of code
#this approach is a "sampling model"
X <- sample(c(-1,1), n,replace = TRUE, prob=c(9/19,10/19))
S <- sum(X)
S

#we can create an estimated distribution function by using a monte carlo simulation
n <- 1000
B <- 10000
S <- replicate(B,{
    X <- sample(c(-1,1), n,replace = TRUE, prob=c(9/19,10/19))
    sum(X)
})

#this is a very good approximation of the cumulative distribution function
mean(S<0)

#plotting the result we can see the distribution is relatively normal and 
hist(S)

#and now that we know that it is normal we can take the mean and sd to add a normal density with these to our original histogram
avgS<-mean(S)
sdS<-sd(S)
s<-seq(min(S),max(S),length=100)
normal_density <- data.frame(s=s, f=dnorm(s,avgS,sdS))
data.frame(S=S) %>% ggplot(aes(S,..density..)) +
    geom_histogram(color = "black",binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping=aes(s,f),color = "blue")