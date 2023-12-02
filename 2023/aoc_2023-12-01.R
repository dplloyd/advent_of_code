# 01-12-2023 Advent of Code
# 

library(tidyverse)

# Part 1

data <- readr::read_table("2023/data/aoc_data_2023-12-01_pt1",col_names = FALSE) |> as_vector()

#get the numbers
numbers_only <- data.frame(numbers = stringr::str_extract_all(data,"\\d+") |> 
sapply(paste0, collapse = "")  )

# Get the left and right numbers
numbers_only <- numbers_only |> 
  mutate(first = str_extract(numbers, "^\\d"),
         last = str_extract(numbers, "\\d$")
)

# Build a new number from the first and last digits, and sum
numbers_only <- numbers_only |>
  mutate(combined = as.numeric(paste0(first,last)))
  
  sum(numbers_only$combined)
  
  
  # Part 2
  
data_pt2 <- sapply(data,paste0, collapse = "")

# define a function to replace text with numbers
replace_words_with_numbers <- function(string) {
  string <- str_replace_all(string, "one", "1") |>
    str_replace_all("two", "2") |>
    str_replace_all("three", "3") |>
    str_replace_all("four", "4") |>
    str_replace_all("five", "5") |>
    str_replace_all("six", "6") |>
    str_replace_all("seven", "7") |>
    str_replace_all("eight", "8") |>
    str_replace_all("nine", "9")
  
  return(string)
}

test <- "onetwo34"

words_replaced <- replace_words_with_numbers(data_pt2)

#get the numbers
numbers_only_pt2 <- data.frame(numbers = stringr::str_extract_all(words_replaced,"\\d+") |> 
                             sapply(paste0, collapse = "")  )
head(numbers_only_pt2)

# Get the left and right numbers
numbers_only_pt2 <- numbers_only_pt2 |> 
  mutate(first = str_extract(numbers, "^\\d"),
         last = str_extract(numbers, "\\d$")
  )

# Build a new number from the first and last digits, and sum
numbers_only_pt2 <- numbers_only_pt2 |>
  mutate(combined = as.numeric(paste0(first,last)))

sum(numbers_only_pt2$combined)


