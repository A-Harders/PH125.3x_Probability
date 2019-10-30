#MONTE CARLO SIMULATIONS
#first we use the rep() to create the urn with 2 red and 3 blues
beads <- rep( c("red","blue"), times = c(2,3))
beads

#then we can use the sample function to pick one at random
sample(beads, 1)

#to create a real monte carlo simulation we use replicate() as this repeats the contained function a specified number of times
B <- 10000
events <- replicate(B, sample(beads, 1))
events

#we can use table to see the distribution of our simulation and if our definition is in agreement with the approximation
tab <- table(events)
tab

#we can use prop.table() to give us the proportions
prop.table(tab)

#sample() doesnt replace by default, and will throw an error if it is asked to sample more than there are possibilities
sample(beads, 6)

#sample can be configured with replace and we can then use it as many times as we like
sample_event <- sample(beads, 8, replace = TRUE)
prop.table(table(sample_event))