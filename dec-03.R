library(tidyverse)

### ADVENT OF CODE ###
### DAY 3

#---- PART 1 ----#

data <- read_table2("data/dec-03-data.txt",
                    col_names = FALSE) %>%
  as_tibble()

data_split  <-
  map_dfc(data$X1, .f =  ~ strsplit(as.character(.), "")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(
    total  = rowSums(.),
    gamma = if_else(total <= 500, 0, 1),
    epsilon = if_else(gamma == 1, 0, 1)
  ) %>%
  select(total, gamma, epsilon) %>%
  t() %>%
  as_tibble() %>%
   mutate_if(is.numeric,as.character) %>%
  unite("solution", V1:V12, sep = "")

# convert from binary to decimal
solution <- strtoi(data_split$solution, base = 2)
solution[2]*solution[3]


#---- PART 2 ----#


