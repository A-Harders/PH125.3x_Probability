#the game show has 3 doors one has a prize, if you pick incorrectly the one you didnt pick that loses is opened
#here is the conundrum, do you stay or change your choice? Logic says the odds are the same, but in reality change is 2/3 and remain is 1/3
#here is a simulation to show the strategy of sticking to the same door
B <- 10000
stick <- replicate(B,{
    doors <- as.character(1:3)
    prize <- sample(c("car","goat","goat"))
    prize_door <- doors[prize == "car"]
    my_pick <- sample(doors,1)
    show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
    stick <- my_pick
    stick == prize_door
})
mean(stick)

#here is a simulation to show the strategy of changing
change <- replicate(B,{
    doors <- as.character(1:3)
    prize <- sample(c("car","goat","goat"))
    prize_door <- doors[prize == "car"]
    my_pick <- sample(doors,1)
    show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
    stick <- my_pick
    change <- doors[!doors %in% c(my_pick,show)]
    change == prize_door
})
mean(change)