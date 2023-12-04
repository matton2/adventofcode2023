library(tidyverse)
library(adventdrob)


ex <- read_lines('day3/day3Ex.txt')
exDF <- read_csv('day3/day3Ex.txt', col_names = 'x')
puzzle <- read_lines('day3/day3Puzzle.txt')
puzzleDF <- read_csv('day3/day3Ex.txt', col_names = 'x')

temp <- exDF |>
    grid_tidy(x) |>
    adjacent_join()


part1 <- function(input) {

    symbols <- c('')

    for(i in 1:length(input)) {
        temp <- str_replace_all(ex[2], "\\.", " ")
        specials <- str_locate_all(temp, "[:punct:]")
        if(NROW(specials[[1]]) == 0) {
            break
        }

    }

}
