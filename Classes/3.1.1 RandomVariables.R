#random variables are numeric outcomes from random processes
beads <- rep( c("red","blue"), times = c(2,3))
x <- replicate(3,{
    ifelse(sample(beads,1) =="blue",1,0)
})
x