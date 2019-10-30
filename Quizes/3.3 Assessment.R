#confg
options(digits = 3)

#SAT QUESTION 1
    #Question 1 - probability of guessing an SAT question correctly
    correct_answer <- 1/5
    incorrect_answer <- 1 - correct_answer

    #Question 2 - expected value of single SAT question
    (1*correct_answer)+((-0.25)*incorrect_answer)

    #Question 3 - expected  value of SAT exam
    ((1*correct_answer)+((-0.25)*incorrect_answer))*44

    #Question 4 - standard error of guessing all 44
    sqrt(44)*(1- -0.25)*sqrt(correct_answer*incorrect_answer)

    #Question 5 - CLT to detemine probability of guessing 8 points or higher
    1-pnorm(8,0,3.32)

    #Question 6 - Monte Carlo of guessing 8
        set.seed(21, sample.kind = "Rounding")
        B <- 10000
        S <- replicate(B,{
            X <- sample(c(1,-.25),44,replace = TRUE,prob = c(correct_answer,incorrect_answer))
            sum(X)
        })

        1-pnorm(8,mean(S),sd(S))

#SAT QUESTION 2
    #Question 1 - expected value of the new SAT exam when guessing
    correct_answer <- 1/4
    incorrect_answer <- 1 - correct_answer
    ((1*correct_answer)+(0*incorrect_answer))

    #Question 2 - probability of scoring over 30 when guessing
    sigma <- sqrt(44)*abs(-1)*sqrt(0.25*0.75)
    mu <- ((1*0.25)+(0*0.75))*44

    1-pnorm(30,mu,sigma)

    #Question 3 - lowest range that the probability of scoring over 35 exceeds 80%
    p <- seq(0.25,.95,0.05)
    prob <- function(n){
        ev <- (1*n)*44
        data.frame(1-pnorm(35,ev,sigma),n)
    }
    sapply(p,prob)

#ROULETTE QUESTION 3
    #Question 1 - mu of the house special ($6 @ 5/38)
    winning <- 5/38
    losing <- 1-winning
    mu <- (6*winning)+(-1*losing)
    mu

    #Question 2 - sigma of the house special
    sigma <- sqrt(1)*abs((-1)-(6))*sqrt(winning*losing)
    sigma

    #Question 3 - mu of the average payout over 500 bets
    #because of the CLT the mu of the average is the same as the mu of the sum
    avmu <- (6*winning)+(-1*losing)
    avmu

    #Question 4 - sigma of the average
    avsigma <- (abs((-1)-(6))*sqrt(winning*losing))/sqrt(500)
    avsigma

    #Question 5&6 - average and SE of the sum
    summu <- mu*500
    sumsigma <- sigma*sqrt(500)

    #Question 7 - probablity of losing money over 500 bets
    1-pnorm(0,summu,sumsigma)