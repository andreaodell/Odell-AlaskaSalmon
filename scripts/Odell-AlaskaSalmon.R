
### Read in file and run packages
master <- read.csv("data/master_duplicate_removed.csv")
distances <- read.csv("data/creek_midpoint.csv")
library(tidyverse)

## adding a column for distance of fish based on location_section
master_with_distance <-  cbind(master,midpoint=rep(master$location_section))
### replace section location values in midpoint column with midpoint values.
master_with_distance$midpoint <- distances$midpoint[match(master_with_distance$midpoint, distances$section)]

#remove unnecessary data in Sex column to have only male and female
tolower <- master_with_distance$Sex
target <- c("f", "m")
master_with_distance <- master_with_distance %>% 
  filter(Sex %in% target)


### How many fish per year
fish_per_year <- master_with_distance %>% 
  group_by(Year)

ggplot(fish_per_year, aes(x = Year, color = Sex)) +
  geom_histogram()

#####################################################################
#####################################################################

#Looking at return times and distance in each year between each creek

#####################################################################
#####################################################################


# A creek fish in *year* 
acreek_fish_2005_return_time <- master_with_distance %>% 
  filter(Year == "2005") %>% 
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "a")

# C creek fish in *year*
ccreek_fish_2010_return_time <- master_with_distance %>% 
  filter(Year == "2010") %>% 
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "c")

## create a scatter plot graph
ggplot(acreek_fish_2005_return_time, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point()
ggplot(ccreek_fish_2010_return_time, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point()

