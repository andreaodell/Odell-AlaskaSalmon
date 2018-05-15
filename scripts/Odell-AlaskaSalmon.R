
### Read in file and run packages
master <- read.csv("data/master_duplicate_removed.csv")
distances <- read.csv("data/creek_midpoint.csv")
library(tidyverse)

## adding a column for distance of fish based on location_section
master_with_distance <-  cbind(master,midpoint=rep(master$location_section))
### replace section location values in midpoint column with midpoint values.
master_with_distance$midpoint <- distances$midpoint[match(master_with_distance$midpoint, distances$section)]


### Looking at return times and distance in each year between each creek

# A creek fish in 2004 
acreek_fish_2004_return_time <- master %>% 
  filter(Year == "2004") %>% 
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "a")

# C creek fish in 2004
ccreek_fish_2004_return_time <- master %>% 
  filter(Year == "2004") %>% 
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "c")



