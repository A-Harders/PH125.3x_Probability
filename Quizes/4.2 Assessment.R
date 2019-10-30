#config
options(digits = 3)
library(tidyverse)
library(dslabs)

#QUESTION 1 - DEATH_PROB DATA FEMALE
    data(death_prob)

    #Question 1 - find the prob of 50 yo females
        head(death_prob)
        f_prob <- death_prob %>%
            filter(.$sex == "Female" & age == 50) %>%
            .$prob
        f_prob

    #Question 2 - mu of policy on 50 yo females
        l <- -150000
        g <- 1150
        n <- 1000
        mu <- f_prob*l + (1-f_prob)*g

    #Question 3 - sigma of policy on 50 yo female
        se <- (g-l)*sqrt(f_prob*(1-f_prob))
        se

    #Question 4 - mu of 1000 policies
        n*mu

    #Question 5 - sigma of 1000 policies
        sqrt(n)*se

    #Question 6 - CLT prob insurance company loses money
        pnorm(0,667378,269658)

#QUESTION 2 - DEATH_PROB DATA MALE
    data(death_prob)

    #Question 1 - find the prob of 50 yo male
        m_prob <- death_prob %>%
            filter(.$sex == "Male" & age == 50) %>%
            .$prob
        m_prob

    #Question 2 - what premium needs to be paid over 1000 policies for $700k profit
        mu <- 700000
        n <- 1000
        p <- m_prob
        a <- -150000
        b <- (mu/n - a*p)/(1-p)
        b

    #Question 3 - Sigma of the new premium rate
        sigma <- sqrt(n) * ((b - a)*sqrt(p*(1-p)))
        sigma

        pnorm(0,mu,sigma)

#QUESTION 3 - DEATH_PROB DATA + CATASTROPHIC PANDEMIC (0.015)
    data(death_prob)

    #Question 1 - expected value of a 50 yo over 1000 policies
        p <- 0.015
        a <- -150000
        b <- 1150
        n <- 1000
        mu <- n*((p*a) + ((1-p)*b))
        mu

    #Question 2 - standard error of a 50 yo over 1000 policies
        sigma <- sqrt(n) * (b-a) * sqrt(p*(1-p))
        sigma

    #Question 3 - probability of losing money
        pnorm(0,mu,sigma)

    #Question 4 - prob of 1 mil loses
        pnorm(-1000000,mu,sigma)

    #Question 5 - lowest death probability losing money exceeds 90%
        p <- seq(.01,.03,.001)
        sigma_seq <- function(p) {
            se <- sqrt(n) * (b-a) * sqrt(p*(1-p))
            mu <- n*((p*a) + ((1-p)*b))
            data.frame(pnorm(0,mu,se),p)
        }
        sapply(p,sigma_seq)

    #Question 6 - lowest death probability losing 1mil exceeds 90%
        p <- seq(.01,.03,.001)
        sigma_seq <- function(p) {
            se <- sqrt(n) * (b-a) * sqrt(p*(1-p))
            mu <- n*((p*a) + ((1-p)*b))
            data.frame(pnorm(-1000000,mu,se),p)
        }

#QUESTION 4 - DEATH_PROB DATA + CATASTROPHIC PANDEMIC (0.015) CONT
    data(death_prob)
    set.seed(25)

    #Question 1 - create sampling model for total profit over 1000 loans
        p <- 0.015
        a <- -150000
        b <- 1150
        n <- 1000

        x <- sample(c(a,b), n, prob = c(p,1-p), replace = TRUE)
        sum(x)/10^6

    #Question 2 - monte carlo of 10000 replicates new seed prob of losing 1mil
        set.seed(27,sample.kind = "Rounding")
        B <- 10000
        p <- 0.015
        a <- -150000
        b <- 1150
        n <- 1000

        x <- replicate(B, {
            simulated_data <- sample(c(a,b), n, prob = c(p,1-p), replace = TRUE)
            sum(simulated_data)
        })

        mean(x < -1*10^6)

#QUESTION 5 - DEATH_PROB DATA + CATASTROPHIC PANDEMIC (0.015) CONT
    data(death_prob)

    #Question 1 - find the premium so chance of loss is below 5%
        n <- 1000
        p <- 0.015
        l <- -150000
        z <- qnorm(0.05) #indicative of 5%
        x <- -l*( n*p - z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))
        x

    #Question 2 - mu of this rate
        mu <- (p*l+(1-p)*x)
        mu

    #Question 3 - mu over 1000 policies
        mu * n

    #Question 4 - monte carlo of loss on losing money
        set.seed(28)
        B <- 10000
        
        profit <- replicate(B,{
            simulation <- sample(c(l,x),n,replace = TRUE, prob = c(p,(1-p)))
            sum(simulation)
        })

        mean(profit<0)

#QUESTION 6 - DEATH_PROB DATA + CATASTROPHIC PANDEMIC (0.015) + UNCERTAINTY +-(0.01)
    data(death_prob)
    set.seed(29, sample.kind = "Rounding")

    #Function for answers
        B <- 10000
        n <- 1000
        p <- 0.015
        l <- -150000
        z <- qnorm(0.05) #indicative of 5%
        x <- -l*( n*p - z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))        

        profit <- replicate(B,{
            new_p <- 0.015 + sample(seq(-0.01,0.01,length = 100),1)
            simulation <- sample(c(l,x),n,replace = TRUE, prob = c(new_p,(1-new_p)))
            sum(simulation)
        })        

    #Question 1 - expected value over 1000 policies
        mean(profit)

    #Question 2 - probability of losing money
        mean(profit<0)

    #Question 3 - prob loss greater than a mil
        profit <- replicate(B,{
            new_p <- 0.015 + sample(seq(-0.01,0.01,length = 100),1)
            simulation <- sample(c(l,x),n,replace = TRUE, prob = c(new_p,(1-new_p)))
            sum(simulation)/10^6
        })  

        mean(profit < -1)