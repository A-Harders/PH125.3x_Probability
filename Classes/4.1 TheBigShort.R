#config
library(tidyverse)
library(dslabs)

#4.1.1 INTEREST RATES EXPLAINED
    #suppose you are a bank calculating interest rates, you give out 1000 loans at 180k and each default costs 200k
    n <- 1000
    loss_per_foreclosure <- -200000
    p <- 0.02 #foreclosure rate
    defaults <- sample (c(0,1),n,prob=c(1-p, p), replace = TRUE)
    sum(defaults * loss_per_foreclosure)

    #now we construct a monte carlo simulation to det an idea of the distribution of this random defaulting variable
    B <- 10000
    losses <- replicate(B,{
        defaults <- sample (c(0,1),n,prob=c(1-p, p), replace = TRUE)
        sum(defaults * loss_per_foreclosure)
    })

    data.frame(losses_in_millions = losses/10^6) %>%
        ggplot(aes(losses_in_millions)) +
        geom_histogram(binwidth = 0.6,col = "black")

    #we dont really need the monte carlo though as the CLT tells us that the because our losses are a sum of independent draws
    #its distributions is approximately normal, as such we can calculate the expected value and standard deviation
    mu <- n*(p*loss_per_foreclosure + (1-p)*0)
    sigma <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

    #as we are making a loss we need to add interest on each loan to, on average, break even
    #this takes the mu formula and computes the interest that needs to be charged on the (1-p) event
    - loss_per_foreclosure*p/(1-p)

    #breaking even isnt just enough because that means that we still have a 50% chance of making a loss
    #we cant set our interest rates too high as our clients will leave, so we need to take on some risk
    #assuming we accept a risk of losing 1 in 100
    #algebraic formula in the workbook for the below formula
    n <- 1000 #number of loans
    p <- 0.02 #foreclosure rate
    l <- loss_per_foreclosure
    z <- qnorm(0.01)
    x <- -l*( n*p - z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))
    x

    #now also note that we have an expected profit per loan of ~2124
    loss_per_foreclosure*p + x*(1-p)

    #running a monte carlo we can confirm that we stand to make a profit of around 2 million
    B <- 10000
    profit <- replicate(B, {
        loans <- sample( c(x,loss_per_foreclosure), n,
                        prob = c(1-p,p),replace = TRUE)
        sum(loans)
    })
    mean(profit)
    mean(profit<0)

#4.1.2 THE BIG SHORT
    #a bank employee notes that we should do more loans to make more money
    #we explain our loans are carefully selected to minimise the liklihood of defaults
    #he states that even if the defaults were higher, if the mu is positive, we can use the law of large numbers to minimise losses
    #if we set interest at 5% the default rate can increase to 4% and we still make money
    r <- 0.05
    x <- r*180000
    p <- 0.04
    loss_per_foreclosure*p + x*(1-p)

    #as we know the risk, the mu and the sigma we can calculate the n with an algebraic term to use the law of large numbers to minimise the chance of loss
    #NOTE: actual algebraic expressions in the notebook
    z <- qnorm(0.01)
    n <- ceiling( (z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2 )
    
    #with this new n amount we can calculate the profits
    n*(loss_per_foreclosure*p + x *(1-p))

    #which can be confirmed in a monte carlo
    B <- 10000
    p <- 0.04
    x <- 0.05*180000
    profit <- replicate(B, {
        draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob = c(1-p,p),replace = TRUE)
        sum(draws)
    })
    mean(profit<0)

    #your employee quits and starts his own bank of high-risk loans but quickly goes bankrupt because he though all the x's were independent
    #here we calculate the probability of defaults goes up and down depending on world trends and affect all mortgages
    p <- 0.04
    x <- 0.05*180000
    profit <- replicate(B, {
        new_p <- 0.04 + sample(seq(-0.01,0.01,length = 100),1)
        draws <- sample( c(x, loss_per_foreclosure), n,
                            prob = c(1-new_p, new_p),replace = TRUE)
    })
    mean(profit)
    mean(profit <-1*10^6)