#we can create a function that creates a lookup table of probabilities for the birthday problem
compute_prob <- function(n, B=10000){
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace=TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}

n <- seq(1,60)

#for loops are not the preferred method of computaion, we prefer to operate on entire vectors of sequences
#many arithmetic functions operate on vectors as sequences regardless, but some functions expecting a scalar wont by default
sqrt(n)

#sapply() allows us to run element wise operations on all functions
prob <- sapply(n, compute_prob)
plot(prob)

#whilst the monte carlo is useful we want to compute the actual probability for 2 reasons
    #we get an actual probability not an approximation based on simulation
    #it is faster to process the mathematics instead of running simulations
exact_prob <- function(n){
    prob_unique <- seq(365,365-n-1)/365
    1-prod(prob_unique)
}

eprob <- sapply(n, exact_prob)
plot(n,prob)
lines(n,eprob,col='red')