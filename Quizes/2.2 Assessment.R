#config
    library(tidyverse)
    library(dslabs)
    set.seed(16, sample.kind = "Rounding")
    act_scores <- rnorm(10000,20.9,5.7)
    options(digits = 3)

#Question 1 - Mean of ACT to confirm generation (saved for functions)
    avg <- mean(act_scores)

#Question 2 - Standard Deviation (saved for functions)
    s <- sd(act_scores)

#Question 3 - # of perfect scores (<36)
    data.frame(act_scores = act_scores) %>%
        filter(act_scores >= 36) %>%
        nrow()

#Question 4 - Probability greater than 30
    1-pnorm(30, avg, s)

#Question 5 - Probability less than equal to 10
    pnorm(10, avg, s)

#Question 6 - plotting upto 36
    x <- seq(1:36)
    x_f <- dnorm(x,20.9,5.7)
    plot(x, x_f)

#Question 7 - create zscores probability of Z-score greater than 2
    act_zscores <- (act_scores-avg)/s
    zavg <- mean(act_zscores)
    zs <- sd(act_zscores)
    1-pnorm(2,zavg,zs)

#Question 8 - ACT score associate to 2 SDs above the mean
    avg+(2*s)

#Question 9 - 97.5th percentile of act_scores
    qnorm(0.975, avg,s)

#Question 10 - 4 PARTER: to create a CDF of ACT scores , min integer score for 0.95
    qnorm(0.05, avg,s)

#Question 11 - the actual 0.95 percentile score
    qnorm(0.95, avg,s)

#Question 12 - quantile for  26
    p <- seq(0.01,0.99,0.01)
    sample_quantities <- quantile(act_scores,probs = p)
    data.frame(filter(sample_quantities == 26))

#Question 13 - theoretical quantities and plot for Q-Q
    theoretical_quantities <- qnorm(p,20.9,5.7)
    plot(theoretical_quantities,sample_quantities)