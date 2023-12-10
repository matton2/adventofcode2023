
library(tidyverse)

ex <- read_lines('day9/da9Ex.txt')
puzzle <- read_lines('day9/da9Puzzle.txt')

part1 <- function(input) {

    nextNumbers <- c()

    for(i in seq_along(input)) {

        numbers <- as.numeric(str_split(input[i], " ")[[1]])

        nextNumbers <- c(nextNumbers, figureOutSeq(numbers))

    }

    return(nextNumbers)

}

figureOutSeq <- function(seq) {

    if(all(seq == 0)) {
        return (0)
    } else {

        nextSeq <- c()

        for(i in 1:length(seq)-1) {
            nextSeq[i] <- seq[i+1] - seq[i]
        }

        nextNumber <- seq[length(seq)] + Recall(nextSeq)

        return(nextNumber)

    }

}


numbers <- as.numeric(str_split(puzzle[7], " ")[[1]])

debugonce(figureOutSeq)

figureOutSeq(numbers)


part1(ex)
solve1 <- part1(puzzle)
sum(solve1)


### part 2


part2 <- function(input) {

    nextNumbers <- c()

    for(i in seq_along(input)) {

        numbers <- rev(as.numeric(str_split(input[i], " ")[[1]]))

        nextNumbers <- c(nextNumbers, figureOutSeq(numbers))

    }

    return(nextNumbers)


}


figureOutSeqBackwards <- function(seq) {

    if(all(seq == 0)) {
        return (0)
    } else {

        nextSeq <- c()

        for(i in length(seq):2) {
            n <- seq[i] - seq[i-1]
            nextSeq <- c(nextSeq, n)
        }

        nextNumber <- seq[1] - Recall(rev(nextSeq))

        return(nextNumber)

    }

}

debug(figureOutSeqBackwards)

figureOutSeqBackwards(numbers)

solve2 <- part2(puzzle)

sum(solve2)
