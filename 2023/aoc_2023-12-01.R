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
  
data_pt2 <- tibble(readr::read_table("2023/data/aoc_data_2023-12-01_pt1",col_names = FALSE) ) |> rename(data = X1)

# define a function to replace text with numbers
replace_words_with_numbers <- function(string) {
  
  # Find the first and last instance of a number word
  
  string_repeated <- rep(string,9)
  number_words <- c("one","two","three","four","five","six","seven","eight","nine")
  number_locs <- str_locate(string_repeated,number_words) |> as_tibble() 
  number_locs$numberword <- number_words
  number_locs$number <- seq(1,9) |> as.character()
  
  first_num_to_replace <-
    number_locs |> filter(start == min(start, na.rm = TRUE)) 
  last_num_to_replace <-
    number_locs |> filter(end == max(end, na.rm = TRUE)) 
  
  if (length(first_num_to_replace) != 0) {
    string <-
      str_replace(string,
                  first_num_to_replace$numberword,
                  first_num_to_replace$number)
  }
  if (length(last_num_to_replace) != 0) {
    string <-
      str_replace_all(string,
                      last_num_to_replace$numberword,
                      last_num_to_replace$number)
  }
  
  return(string)
}

words_replaced <- sapply(data_pt2$data,replace_words_with_numbers) |> 
  unlist()

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


