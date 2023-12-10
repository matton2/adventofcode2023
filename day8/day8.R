
library(tidyverse)

ex1 <- read_lines('day8/day8Ex1.txt')
ex2 <- read_lines('day8/day8Ex2.txt')
ex3 <- read_lines('day8/day8Ex3.txt')
puzzle <- read_lines('day8/day8Puzzle.txt')

part1 <- function(input) {

    instructions <- str_split(input, "")[[1]]

    directionTibble <- tibble(input = input[3:length(input)]) |>
        separate(input, into = c('root', 'dir'), sep = "=") |>
        separate(dir, into = c('L', 'R'), sep = ",") |>
        mutate(root = str_remove_all(root, " "),
               L = str_remove_all(L, "[:punct:]"),
               L = str_remove_all(L, " "),
               R = str_remove_all(R, "[:punct:]"),
               R = str_remove_all(R, " "))

    nextNode <- "AAA"
    nextDirection <- 0
    totalNodes <- 0

    while(nextNode != "ZZZ") {

        nextDirection <- totalNodes + 1

        if(nextDirection > length(instructions)) {
            instructions <- c(instructions, instructions)
        }

        whichWay <- instructions[nextDirection]

        nextNode <- filter(directionTibble, root == nextNode) |>
            pull({{whichWay}})

        totalNodes <- totalNodes + 1

    }

    return(totalNodes)

}

debugonce(part1)

part1(ex1)
part1(ex2)
part1(puzzle)


## Brute force this thing would have taken an age

part2 <- function(input) {

    instructions <- str_split(input, "")[[1]]

    directionTibble <- tibble(input = input[3:length(input)]) |>
        separate(input, into = c('root', 'dir'), sep = "=") |>
        separate(dir, into = c('L', 'R'), sep = ",") |>
        mutate(root = str_remove_all(root, " "),
               L = str_remove_all(L, "[:punct:]"),
               L = str_remove_all(L, " "),
               R = str_remove_all(R, "[:punct:]"),
               R = str_remove_all(R, " "))

    nextNodes <- directionTibble |>
        filter(str_detect(root, "A$")) |>
        pull(root)

    while(!all(str_detect(nextNodes, "Z$"))) {

        nextDirection <- totalNodes + 1

        if(nextDirection > length(instructions)) {
            instructions <- c(instructions, instructions)
        }

        whichWay <- instructions[nextDirection]

        for(i in seq_along(nextNodes)) {
            nextNode <- filter(directionTibble, root == nextNodes[i]) |>
                pull({{whichWay}})

            nextNodes[i] <- nextNode
        }

        totalNodes <- totalNodes + 1

    }

    return(totalNodes)


}

debugonce(part2)

part2(ex3)
part2(puzzle)


### LCM Method

part2LCM <- function(input) {

    instructions <- str_split(input, "")[[1]]

    directionTibble <- tibble(input = input[3:length(input)]) |>
        separate(input, into = c('root', 'dir'), sep = "=") |>
        separate(dir, into = c('L', 'R'), sep = ",") |>
        mutate(root = str_remove_all(root, " "),
               L = str_remove_all(L, "[:punct:]"),
               L = str_remove_all(L, " "),
               R = str_remove_all(R, "[:punct:]"),
               R = str_remove_all(R, " "))

    nextNodes <- directionTibble |>
        filter(str_detect(root, "A$")) |>
        pull(root)

    allTotalNodes <- c()

    for(i in seq_along(nextNodes)) {

        totalNodes <- 0

        while(!str_detect(nextNodes[i], "Z$")) {

            nextDirection <- totalNodes + 1

            if(nextDirection > length(instructions)) {
                instructions <- c(instructions, instructions)
            }

            whichWay <- instructions[nextDirection]

                nextNode <- filter(directionTibble, root == nextNodes[i]) |>
                    pull({{whichWay}})

                nextNodes[i] <- nextNode

            totalNodes <- totalNodes + 1

        }

        allTotalNodes[i] <- totalNodes

    }

    return(allTotalNodes)


}


debugonce(part2LCM)
exLCM <- part2LCM(ex3)

tictoc::tic()
lcmNext <- part2LCM(puzzle)
finalAns <- Reduce(pracma::Lcm, lcmNext)
format(finalAns, scientific = FALSE)
tictoc::toc()
