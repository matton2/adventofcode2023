
library(tidyverse)

ex <- read_lines('day4/day4Ex.txt')
puzzle <- read_lines('day4/day4Puzzle.txt')

part1 <- function(input) {
    # winning numbers on the left side of the | my numbers on the right

    pts <- 0

    for(i in 1:length(input)) {
        temp <- str_split(input[i], ":")
        numbers <- str_split(temp[[1]][2], "\\|")
        winningNumbers <- as.numeric(str_extract_all(numbers[[1]][1], "[0-9.]+")[[1]])
        myNumbers <- as.numeric(str_extract_all(numbers[[1]][2], "[0-9.]+")[[1]])
        matches <- sum(duplicated(c(winningNumbers, myNumbers)))

        cardValue <- 0

        if (matches == 1) {
            cardValue <- 1
        } else if (matches > 1) {
            cardValue <- 1

            for(j in 2:matches) {
                cardValue <- cardValue * 2
            }
        }


        pts <- pts + cardValue
    }

    return(print(paste('Total Value: ', pts)))


}

debugonce(part1)

part1(ex)
part1(puzzle)


part2 <- function(input, myGameCards, totalCards = 0) {

    for(i in seq_along(input)) {
        temp <- str_split(input[i], ":")
        gameNumber <- as.numeric(str_extract_all(temp[[1]][1], "[:digit:]{1,3}")[[1]])
        numbers <- str_split(temp[[1]][2], "\\|")
        winningNumbers <- as.numeric(str_extract_all(numbers[[1]][1], "[0-9.]+")[[1]])
        myNumbers <- as.numeric(str_extract_all(numbers[[1]][2], "[0-9.]+")[[1]])
        allNumbers <- c(winningNumbers, myNumbers)
        doIHavematches <- any(duplicated(allNumbers))
        matches <- NULL
        if(doIHavematches) {
            matches <-c(1:sum(duplicated(allNumbers))) + gameNumber
        }

        if(doIHavematches) {
            for(j in 1:length(matches)) {
                # can i just increase that game count and then run the algorthim again
                totalCards <- totalCards + Recall(myGameCards[matches[j]], myGameCards, totalCards)
            }
            return(totalCards)
        } else {
            return(1)
        }



    }


}

debug(part2)

part2(ex,ex)


part2TibbleVersion <- function(input, myGameCards) {

    for(i in seq_along(input)) {
        temp <- str_split(input[i], ":")
        gameNumber <- as.numeric(str_extract_all(temp[[1]][1], "[:digit:]{1,3}")[[1]])
        numbers <- str_split(temp[[1]][2], "\\|")
        winningNumbers <- as.numeric(str_extract_all(numbers[[1]][1], "[0-9.]+")[[1]])
        myNumbers <- as.numeric(str_extract_all(numbers[[1]][2], "[0-9.]+")[[1]])
        allNumbers <- c(winningNumbers, myNumbers)
        doIHavematches <- any(duplicated(allNumbers))
        matches <- NULL
        if(doIHavematches) {
            matches <-c(1:sum(duplicated(allNumbers))) + gameNumber
        }

        if(doIHavematches) {
            exTibble <<- exTibble |>
                mutate(count = if_else(game == gameNumber, count + 1, count))
            for(j in 1:length(matches)) {
                # can i just increase that game count and then run the algorthim again
               Recall(myGameCards[matches[j]], myGameCards)
            }
        } else {
            exTibble <<- exTibble |>
                mutate(count = if_else(game == gameNumber, count + 1, count))

            exTibble <<- exTibble |>
                mutate(howManyAmIWorth = is_else(howManyAmIWorth == 0, count, howManyAmIWorth))
        }



    }


}


exTibble <- tibble(game = c(1:length(ex)), count = 0, howManyAmIWorth = 0)

debugonce(part2TibbleVersion)

part2TibbleVersion(ex, ex)
sum(exTibble$count)




puzzleTibble <- tibble(game = c(1:length(puzzle)), count = 0, howManyCards = 0)
part2TibbleVersion(puzzle, puzzle)
sum(puzzleTibble$count)


# 5747443
