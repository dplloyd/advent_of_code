library(tidyverse)
library(readr)

# PART 1
# Reading the data
data <- read_table2("data/dec-02-data.txt", 
                           col_names = FALSE) 

colnames(data) <- c("direction","magnitude")


data2 <- data %>% group_by(direction) %>% 
  summarise(mag_sum = sum(magnitude)) %>% 
  pivot_wider(values_from = mag_sum, names_from = direction) %>% 
  mutate(net_y = down - up,
         solution = net_y * forward)
  

data2
data2$solution


# PART 2
data3 <- data %>% mutate(depth = 0, aim = 0,horizontal=0)


# as we start with forward, not going to worry about 0 index
for (i in 1:nrow(data3)) {
  if (i == 1) {
    prev_depth <- 0
    prev_aim <- 0
    prev_x <- 0
  }
  else{
    prev_depth <- data3$depth[i - 1]
    prev_aim <- data3$aim[i - 1]
    prev_x <- data3$horizontal[i - 1]
  }
  
  # amount to increase depth, aim and x
  
  if (data3$direction[i] == "down") {
   delta_aim <-  data3$magnitude[i]
   delta_depth <- 0
   delta_x <- 0
  }
  else if (data3$direction[i] == "up") {
    delta_aim <-  -1*data3$magnitude[i]
    delta_depth <- 0
    delta_x <- 0
  }
  else if (data3$direction[i] == "forward") {
    delta_x <- data3$magnitude[i]
    delta_aim <- 0
    delta_depth <- prev_aim * data3$magnitude[i]
   
  }
  
  data3$aim[i] <- prev_aim + delta_aim
  data3$depth[i] <- prev_depth + delta_depth
  data3$horizontal[i] <- prev_x + delta_x
  

}


data3 %>% tail(1) %>% mutate(solution  = depth * horizontal)

