#this important pieces of code sets the random seed
set.seed(1986)

#an R update from 3.5 to 3.6 changed the default method of setting seed changes
#this means that results may vary from users of 3.5 (more importantly the course) and users of 3.6
#we can set the defuault behavior back to rounding to ensure that we get the same results
set.seed(1, sample.kind ="Rounding")