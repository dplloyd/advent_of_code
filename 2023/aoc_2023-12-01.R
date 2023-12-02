# 01-12-2023 Advent of Code
# 

library(tidyverse)

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

