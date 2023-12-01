# day 1 part A

library(tidyverse)

puzzle <- read_csv('day1/day1Puzzle.csv', col_names = c('input'))
ex <- read_csv('day1/day1Ex.csv', col_names = c('input'))

partA <- puzzle |>
    mutate(numbers = str_remove_all(input, "[:alpha:]"),
           numberFormat = as.numeric(case_when(
               nchar(numbers) == 1 ~ paste0(numbers, numbers),
               nchar(numbers) > 2 ~ paste0(str_sub(numbers, 1, 1), str_sub(numbers, nchar(numbers), nchar(numbers))),
               TRUE ~ numbers
           )))

sum(partA$numberFormat)


findNumbers <- function(input) {

    ##input <- 'eightwothree'

   temp <-  str_locate_all(input, c("one"="one",
                             "two"="two",
                             "three"="three",
                             "four"= "four",
                             "five" = "five",
                             "six"= "six",
                             "seven" = 'seven',
                             "eight"= 'eight',
                             "nine" = "nine"))

   numbers <- tibble()

   for(i in 1:length(temp)) {
       stupidList <- as.tibble(temp[[i]]) |>
           mutate(number = i)

       numbers <- bind_rows(numbers, stupidList)
   }

   actualNumbers <- as.tibble(str_locate_all(input, '[:digit:]')[[1]]) |>
       mutate(number = as.numeric(str_sub(input, start, end)))

      cleanNumbers <- numbers |>
        bind_rows(actualNumbers) |>
       filter(!is.na(start)) |>
       arrange(start)

  final <- paste0(cleanNumbers$number, collapse = '')

  if(nchar(final) == 1) {
      final <- paste0(final, final)
  }

   return(final)



}

debugonce(findNumbers)

findNumbers('three28jxdmlqfmc619eightwol')

partBEx <- ex |>
    rowwise() |>
    mutate(numbers = unlist(list(findNumbers(input))),
           #numbers = str_remove_all(letterToNumber, "[:alpha:]"),
           numberFormat = as.numeric(case_when(
               nchar(numbers) > 2 ~ paste0(str_sub(numbers, 1, 1), str_sub(numbers, nchar(numbers), nchar(numbers))),
               TRUE ~ numbers
           )))

sum(partBEx$numberFormat)



partB <- puzzle |>
    rowwise() |>
    mutate(numbers = unlist(list(findNumbers(input))),
           numberFormat = as.numeric(case_when(
               nchar(numbers) > 2 ~ paste0(str_sub(numbers, 1, 1), str_sub(numbers, nchar(numbers), nchar(numbers))),
               TRUE ~ numbers
           )))


sum(partB$numberFormat)

