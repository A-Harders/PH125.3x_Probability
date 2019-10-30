#an extreme example of non independence
#we generate our urn with 2 red and 3 blue beads
beads <- rep(c("red","blue"), times = c(2,3)

#we then sample the beads without replacing them
x <- sample(beads, 5)

#logic tells us that the probability of the first bead is 60% blue
#but if we know the results of the last 4 results it changes the probability of the first
x[2:5]

#now that we know the last results we know with 100% certainty (because the probability is 1) that the first was red, against the odds
#the probability changes once we know the other outcomes