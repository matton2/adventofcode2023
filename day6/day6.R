library(tidyverse)

puzzle <- read_lines("day6/day6Puzzle.txt")

lines <- str_extract_all(puzzle, "[:digit:]{1,10}")

times <- as.numeric(lines[[1]])
distance <- as.numeric(lines[[2]])


part1 <- function(times, distance) {

    howManyWaysToWin <- c()

    for(i in seq_along(times)) {
        raceTime <- times[i]
        raceDistance <- distance[i]

        # i need to hold the button down long enough that i bet the distance mark
        # the longer i hold the botton for the faster i go, for the time allowed
        # hold for 1s go 1m/s, hold for 2s go 2m/s
        # basic math, i hold down for x sec, subtract that from the time, figure out the time left and speed and see how long i go

        totalWins <- 0

        for(j in c(2:raceTime)) {

            speed <- j
            timeLeft <- raceTime - j

            totalDistance <- j * timeLeft

            if(totalDistance > raceDistance) {
                totalWins <- totalWins + 1

            }



        }

        howManyWaysToWin <- c(howManyWaysToWin, totalWins)

    }

    return(prod(howManyWaysToWin))

}

debugonce(part1)

part1(times, distance)


distance2 <- as.numeric(paste(distance, collapse = ""))
times2 <- as.numeric(paste(times, collapse = ""))

part2 <- function(time, distance) {

        raceTime <- time
        raceDistance <- distance

        # i need to hold the button down long enough that i bet the distance mark
        # the longer i hold the botton for the faster i go, for the time allowed
        # hold for 1s go 1m/s, hold for 2s go 2m/s
        # basic math, i hold down for x sec, subtract that from the time, figure out the time left and speed and see how long i go

        totalWins <- 0

        for(j in c(2:raceTime)) {

            speed <- j
            timeLeft <- raceTime - j

            totalDistance <- j * timeLeft

            if(totalDistance > raceDistance) {
                totalWins <- totalWins + 1

            }



        }


    return(totalWins)

}

part2(times2, distance2)
