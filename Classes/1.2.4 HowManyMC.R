#we are never sure of how many Monte Carlos we need to make a good approximation
#one way is to test the stability of the mobel
B <- 10^seq(1,5,len=100)
compute_prob <- function(B,n=22){
    same_day <- replicate(B,{
        bdays <- sample(1:365,n,replace=TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}

prob <- sapply(B, compute_prob)
plot(log10(B),prob, type = "l")