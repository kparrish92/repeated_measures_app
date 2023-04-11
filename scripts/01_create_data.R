library(here)
library(tidyverse)

# sim data to have 1000 participants, a mean of 60 and sd of 25 
data = data.frame(vot = rnorm(1000, 60, 20), participant = 1:100) 

data %>% 
  write.csv(here("data", "data.csv"))