#PREMISE: we are in room with 50 random people, what are the odds that 2 share a birthday?
#we can compute this mathematically but we will start with a Monte Carlo
n <- 50
bday <-sample(1:365,n,replace=TRUE)

#we need to proof if a birthday has been duplicated and we use duplicated() to do so
#it shows TRUE for the second time an element appears in a vector
any(duplicated(bday))

#now that we have laid the ground work we can create our Monte Carlo
B <- 10000
results <- replicate(B,{
    bdays <- sample(1:365,n,replace=TRUE)
    any(duplicated(bdays))
})
mean(results)

#the results were surprisingly high ~97% in the next section we explain why