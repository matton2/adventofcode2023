
library(tidyverse)

ex <- read_lines('day5/day5Ex.txt')
puzzle <- read_lines('day5/day5Puzzle.txt')

part1 <- function(input) {

    seeds <- as.numeric(str_extract_all(input[1], "[:digit:]{1,50}")[[1]])

    results <- c()

    for(i in seq_along(seeds)) {
        results <- c(results, findPath(seeds[i], input))
    }

    return(results)

}

findPath <- function(seed=79, instructions) {

    instructions <- instructions[3:length(instructions)]

    seedFound <- FALSE

    for(i in seq_along(instructions)) {

        #find the match
        if(str_detect(instructions[i], '[:alpha:]')) {
            next
        } else if (nchar(instructions[i]) == 0) {
            seedFound <- FALSE
            next
        } else if (!seedFound) {
            match <- as.numeric(str_extract_all(instructions[i], "[:digit:]{1,50}")[[1]])
            # start position is in 2 place
            # dest position is in 1

            if(between(seed, match[2], match[2] + match[3]-1)) {
                findDest <- seed - match[2]
                if(findDest == 0) {
                    seed <- match[1]
                } else {
                    seed <- findDest + match[1]
                }
                seedFound <- TRUE

            }
        } else {
            next
        }



    }

    return(seed)


}

debugonce(findPath)

findPath(13, ex)

part1(ex)
min(part1(puzzle))


part2 <- function(input) {

    seeds <- as.numeric(str_extract_all(input[1], "[:digit:]{1,50}")[[1]])
    resultsFinal <- c()

    for(i in seq(1, length(seeds), by = 2)) {

        seedRange <- seq(seeds[i], length.out = seeds[i+1])

        resultsTemp <- c()

        for(j in seq_along(seedRange)) {
            resultsTemp <- c(resultsTemp, findPath(seedRange[j], input))
        }

        resultsFinal <- c(resultsFinal, min(resultsTemp))

    }

    return(resultsFinal)

}

debugonce(part2)

part2(ex)
puzResults <- part2(puzzle)

