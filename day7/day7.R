library(tidyverse)

# ex <- read_lines('AdventOfCode2023/day7Ex.txt')
# exTibble <- read_csv('AdventOfCode2023/day7Ex.txt', col_names = 'input') |>
#     separate(input, into = c('hand', 'wager'), sep = " ") |>
#     separate(hand, into = c('zero','first','second', 'thrid', 'fourth', 'fifth'), sep = '', remove = FALSE) |>
#     mutate(wager = as.numeric(wager))

puzzleTibble <-  read_csv('day7/day7Puzzle.txt', col_names = 'input') |>
    separate(input, into = c('hand', 'wager'), sep = " ") |>
    separate(hand, into = c('zero','first','second', 'thrid', 'fourth', 'fifth'), sep = '', remove = FALSE) |>
    mutate(wager = as.numeric(wager))

order <- c('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

figureOutHand <- function(input) {

    currentHand <- str_split(input, "")[[1]]

    dups <- sum(duplicated(currentHand))

    # high card
    if(dups == 0) {
        return(1)
        # one pair
    } else if (dups == 1) {
        return(2)
        # three of a kind (return 4) or 2 pair (return 3)
    } else if (dups == 2) {

        uniqueCards <- unique(currentHand)
        first <- sum(str_count(currentHand, uniqueCards[1]))
        second <- sum(str_count(currentHand, uniqueCards[2]))
        third <- sum(str_count(currentHand, uniqueCards[3]))

        howMany <- max(c(first, second, third))

        if(howMany == 3) {
            return(4)
        } else {
            return(3)
        }

        # four of a kind (return 6) or a full house (return 5)
    } else if (dups == 3) {

        uniqueCards <- unique(currentHand)

        if(sum(str_count(currentHand, uniqueCards[1])) == 1 ||
           sum(str_count(currentHand, uniqueCards[1])) == 4) {
            return(6)
        } else {
            return(5)
        }
        # five of a kind
    } else if (dups == 4) {
        return(7)
    }

}

solveEx <- exTibble |>
    rowwise() |>
    mutate(handValue = figureOutHand(hand)) |>
    ungroup() |>
    arrange(handValue, desc(match(first, order)), desc(match(second, order)),
            desc(match(thrid, order)), desc(match(fourth, order)),
            desc(match(fifth, order))) |>
    mutate(finalRank = row_number(),
           winnings = wager * finalRank)

sum(solveEx$winnings)

solvePuzzle <- puzzleTibble |>
    rowwise() |>
    mutate(handValue = figureOutHand(hand)) |>
    ungroup() |>
    arrange(handValue, desc(match(first, order)), desc(match(second, order)),
            desc(match(thrid, order)), desc(match(fourth, order)),
            desc(match(fifth, order))) |>
    mutate(finalRank = row_number(),
           winnings = wager * finalRank)

sum(solvePuzzle$winnings)


jokerOrder <- c('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')


figureOutHandWithJokers <- function(input) {

    currentHand <- str_split(input, "")[[1]]

    dups <- sum(duplicated(currentHand))

    # i need to figure out J now

    if(all(str_detect(currentHand, "J", negate = TRUE))) {

        # high card
        if(dups == 0) {
            return(1)
            # one pair
        } else if (dups == 1) {
            return(2)
            # three of a kind (return 4) or 2 pair (return 3)
        } else if (dups == 2) {

            uniqueCards <- unique(currentHand)
            first <- sum(str_count(currentHand, uniqueCards[1]))
            second <- sum(str_count(currentHand, uniqueCards[2]))
            third <- sum(str_count(currentHand, uniqueCards[3]))

            howMany <- max(c(first, second, third))

            if(howMany == 3) {
                return(4)
            } else {
                return(3)
            }

            # four of a kind (return 6) or a full house (return 5)
        } else if (dups == 3) {

            uniqueCards <- unique(currentHand)

            if(sum(str_count(currentHand, uniqueCards[1])) == 1 ||
               sum(str_count(currentHand, uniqueCards[1])) == 4) {
                return(6)
            } else {
                return(5)
            }
            # five of a kind
        } else if (dups == 4) {
            return(7)
        }

    } else {

        numberofJokers <- sum(str_count(currentHand, "J"))

        if(numberofJokers == 4) {
            return(7)
        } else if (numberofJokers == 3 && dups == 3) {
            return(7)
            # high card is worth 1
        } else if (dups == 0) {
            return(2)
            # one pair is worth 2
        } else if (dups == 1) {
            return(4)
            # three of a kind (return 4) or 2 pair (return 3)
        } else if (dups == 2) {

            uniqueCards <- unique(currentHand)
            first <- sum(str_count(currentHand, uniqueCards[1]))
            second <- sum(str_count(currentHand, uniqueCards[2]))
            third <- sum(str_count(currentHand, uniqueCards[3]))

            howMany <- max(c(first, second, third))

            if(howMany == 3) {
                return(5 + 1)
            } else if (numberofJokers == 2)  {
                return(5 + 1)
            } else {
                return(3 + 2)
            }

            # four of a kind (return 6) or a full house (return 5)
        } else if (dups == 3) {

            uniqueCards <- unique(currentHand)
            first <- sum(str_count(currentHand, uniqueCards[1]))
            second <- sum(str_count(currentHand, uniqueCards[2]))

            # a four of a kind with joker will turn into a 5 of a kind
            if(max(c(first, second)) == 4) {
                return(7)
            } else if (numberofJokers == 2) {
                return(7)


            } else {
                return(5)
            }
            # five of a kind
        } else if (dups == 4) {
            return(7)
        }
    }


}

# debug(figureOutHandWithJokers)

solve2Ex <- exTibble |>
    rowwise() |>
    mutate(handValue = figureOutHandWithJokers(hand)) |>
    ungroup() |>
    arrange(handValue, desc(match(first, jokerOrder)), desc(match(second, jokerOrder)),
            desc(match(thrid, jokerOrder)), desc(match(fourth, jokerOrder)),
            desc(match(fifth, jokerOrder))) |>
    mutate(finalRank = row_number(),
           winnings = wager * finalRank)


sum(solve2Ex$winnings)

solve2Puzzle <- puzzleTibble |>
    rowwise() |>
    mutate(handValue = figureOutHandWithJokers(hand)) |>
    ungroup() |>
    arrange(handValue, desc(match(first, jokerOrder)), desc(match(second, jokerOrder)),
            desc(match(thrid, jokerOrder)), desc(match(fourth, jokerOrder)),
            desc(match(fifth, jokerOrder))) |>
    mutate(finalRank = row_number(),
           winnings = wager * finalRank)


sum(solve2Puzzle$winnings)
