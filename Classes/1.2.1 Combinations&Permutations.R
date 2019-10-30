#config
install.packages("gtools")
library(gtools)

#we will be using the expand.grid() and paste() to create a deck of cards
#paste() joins 2 strings together, it also works on pairs of vectors
#expand.grid() gives us all of the combinations of 2 lists
numbers <- c("ace","two","three","four","five","six","seven","eight","nine","ten","jack","queen","king")
suits <- c("hearts","diamonds","clubs","spades")

card_deck <- expand.grid(number=numbers,suit=suits)
card_deck <- paste(card_deck$number,card_deck$suit)

#now that we have a deck of cards we can compute probability
kings <- paste("king",suits)
mean(card_deck %in% kings)

#now we compute the conditional probability of the second card also being a king
#to do this now we need to do the combinations() and permutation() functions
#permutations() and combinations() are part of the gtools package
#permutations() computes for any list of n the different ways we can select R
permutations(9,3)

#permutations() can also have a vector added, for example if we wanted to generate random phone numbers
all_phone_numbers <- permutations(10,8,v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

#to compute all possible ways that we can choose 2 cards we can use permutations() like below
hands<-permutations(52,2,v=card_deck)

#we are going to define the first cards and the second cards
first_card <- hands[,1]
second_card <- hands[,2]

#we can now check how many cases for each have a king
sum(first_card %in% kings)
sum(second_card %in% kings)

#and we can use this to show what fraction of these have kings in both
sum(first_card %in% kings & second_card %in% kings) /
    sum(first_card %in% kings)
#NOTE: this is equivelant code we used to compute the proportions instead of totals
mean(first_card %in% kings & second_card %in% kings) /
    mean(first_card %in% kings)

#combinations() computes the permutations but it doesnt concern itself with position
#this is useful for blackjack testing because ace, king is the same as king, ace, and we calculate this with the below code
aces <- paste("ace", suits)

facecard <- c("king","queen","jack","ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2,v=card_deck)

mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#NOTE: we assumed the aces came first because we know the way that combination generates and enumerates possibilities
#a safer way to write the code to test for both possibiliteies is below
mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
        (hands[,2] %in% aces & hands[,1] %in% facecard))

#we can also use a Monte Carlo to estimate this probability, and we use sample() to draw without replacement
hand <- sample(card_deck,2)
B <- 20000
results <- replicate(B,{
    hand<-sample(card_deck,2)
    (hand[1] %in% aces & hand[2] %in% facecard)|
        (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
?replicate()