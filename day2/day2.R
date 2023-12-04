library(tidyverse)

ex <- read_lines('day2/day2Ex.txt')
puzzle <- read_lines('day2/day2Puzzle.txt')

isPossible <- function(input, red = 12, green = 13, blue = 14) {

    ans <- 0

    for(i in 1:length(input)) {
        # second position will always be the game number
        gameNumber <- as.numeric(str_remove(str_split(input[i], " ")[[1]][2], ":"))

        reds <- all(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= red)")[[1]]) <= red)
        greens <- all(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= green)")[[1]]) <= green)
        blues <- all(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= blue)")[[1]]) <= blue)

        if (all(reds, greens, blues)) {
            ans <- ans + gameNumber
        }

    }

    print(paste("Part 1 Answer:", ans))


}

debugonce(isPossible)

isPossible(ex)

isPossible(puzzle)



maxPower <- function(input) {

    ans <- 0

    for(i in 1:length(input)) {
        # second position will always be the game number
        gameNumber <- as.numeric(str_remove(str_split(input[i], " ")[[1]][2], ":"))

        reds <- max(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= red)")[[1]]))
        greens <- max(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= green)")[[1]]))
        blues <- max(as.numeric(str_extract_all(input[i], "[:digit:]{1,2}(?= blue)")[[1]]))

        power <- reds * greens * blues

       ans <- ans + power

    }

    print(paste("Part 2 Answer:", ans))


}

maxPower(ex)
maxPower(puzzle)
