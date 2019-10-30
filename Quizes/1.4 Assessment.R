#config
    library(gtools)
    library(tidyverse)
    options(digits = 3)
    set.seed(1)

#Question 1 - Olympic Running
    #Q1A
        permutations(8,3)

    #Q1B
        permutations(3,3)

    #Q1C
        6/336

    #Q1D
        runners <- rep(c("Jamaica","USA","Ecuador","Netherlands","France","South Africa"),times = c(3,1,1,1,1,1))
        R <- 10000
        race_result <- replicate(B,{
            winners <- sample(runners, 3)
            all(winners == "Jamaica")
        })
        mean(race_result)

#Question 2 - Restaurant Managment
    #Q2A
        entree1 <- nrow(combinations(6,1))
        sides1 <- nrow(combinations(6,2))
        drinks1 <- nrow(combinations(3,1))
        entree1*sides1*drinks1

    #Q2B
        entree2 <- nrow(combinations(6,1))
        sides2 <- nrow(combinations(6,2))
        drinks2 <- nrow(combinations(3,1))
        entree2*sides2*drinks2

    #Q2C
        entree3 <- nrow(combinations(6,1))
        sides3 <- nrow(combinations(6,3))
        drinks3 <- nrow(combinations(3,1))
        entree3*sides3*drinks3

    #Q2D
        B <- seq(1:12)
        sides4 <- nrow(combinations(6,2))
        drinks4 <- nrow(combinations(3,1))

        combination_calc <- function(B){
            no_comb <- replicate(B,{
                entree4 <- combinations(B,1)
                entree4*sides4*drinks4
            })
        }

        sapply(B,combination_calc)

    #Q2E
        B <- seq(2,12)
        entree5 <- nrow(combinations(6,1))
        drinks5 <- nrow(combinations(3,1))

        combination_calc <- function(B){
            no_comb <- replicate(B,{
                sides5 <- nrow(combinations(B,2))
                entree5*sides5*drinks5
            })
        }

        sapply(B,combination_calc)

        B

#Question 3:6 - Oesophageal Cancer
    #config
        head(esoph)

    #Q3A
        nrow(esoph)

    #Q3B
        all_cases <- sum(esoph$ncase)
        all_cases

    #Q3C
        all_controls <- sum(esoph$ncontrols)
        all_controls

    #Q4A
        max_alc = max(esoph$alcgp)
        max_alc_filter <- esoph %>%
            filter(alcgp == max_alc)

        max_alc_cases <- sum(max_alc_filter$ncase)
        max_alc_control <- sum(max_alc_filter$ncontrol)

        max_alc_cases/(max_alc_cases+max_alc_control)

    #Q4B
        min_alc = min(esoph$alcgp)
        min_alc_filter <- esoph %>%
            filter(alcgp == min_alc)

        min_alc_cases <- sum(min_alc_filter$ncase)
        min_alc_control <- sum(min_alc_filter$ncontrol)

        min_alc_cases/(min_alc_cases+min_alc_control)

    #Q4C
        big_tob <- esoph %>%
            filter(tobgp != "0-9g/day")

        big_tob_cases <- sum(big_tob$ncase)
        big_tob_control <- sum(big_tob$ncontrol)

        big_tob_cases/all_cases

    #Q4D
        big_tob <- esoph %>%
            filter(tobgp != "0-9g/day")

        big_tob_cases <- sum(big_tob$ncase)
        big_tob_control <- sum(big_tob$ncontrol)

        big_tob_control/all_controls

    #Q5A
        pr_maxalccas <- max_alc_cases/all_cases

    #Q5B
        max_tob <- esoph %>%
            filter(tobgp == "30+")

        max_tob_cases <- sum(max_tob$ncases)
        max_tob_control <- sum(max_tob$ncontrol)

        max_tob_cases/all_cases

    #Q5C
        max_tob_alc <- esoph %>%
            filter(tobgp == "30+" & alcgp == max(alcgp)) 

        max_tob_alc_cases <- sum(max_tob_alc$ncases)
        max_tob_alc_controls <- sum(max_tob_alc$ncontrol)

        max_tob_alc_cases/all_cases

    #Q5D
        max_tob_or_alc <- esoph %>%
            filter(tobgp == "30+" | alcgp == max(alcgp)) 

        max_tob_or_alc_cases <- sum(max_tob_or_alc$ncases)
        max_tob_or_alc_controls <- sum(max_tob_or_alc$ncontrol)

        pr_maxtobalc_case <- max_tob_or_alc_cases/all_cases

    #Q6A
        pr_maxalccnt <- max_alc_control/all_controls

    #Q6B
        pr_maxalccas/pr_maxalccnt

    #Q6C
        max_tob_control/all_controls

    #Q6D
        max_tob_alc_controls/all_controls

    #Q6E
        pr_maxtobalc_control <- max_tob_or_alc_controls/all_controls

    #Q6F
        pr_maxtobalc_case/pr_maxtobalc_control