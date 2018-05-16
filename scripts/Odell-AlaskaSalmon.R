
### Read in file and run packages
master <- read.csv("data/autocorrelation_sections_corrected_051118_final.csv", na.strings = "NA")
distances <- read.csv("data/creek_midpoint.csv")
library(tidyverse)

## adding a column for distance of fish based on location_section
master_with_distance <-  cbind(master,midpoint=rep(master$location_section))
### replace section location values in midpoint column with midpoint values.
master_with_distance$midpoint <- distances$midpoint[match(master_with_distance$midpoint, distances$section)]


#remove unnecessary data in Location and Sex column to merge similar vectors
master_with_distance$Location[master_with_distance$Location == "A" |
                                master_with_distance$Location == "amouth" |
                                master_with_distance$Location == "am" |
                                master_with_distance$Location == "abeach"] <- "a"
master_with_distance$Location[master_with_distance$Location == "cm" |
                                master_with_distance$Location == "cbeach" |
                                master_with_distance$Location == "cmouth"] <- "c"

tolower <- master_with_distance$Sex
target_sex <- c("f", "m")
master_with_distance <- master_with_distance %>% 
  filter(Sex %in% target_sex)


### How many fish per year showing m:f ratio
fish_per_year <- master_with_distance %>% 
  group_by(Year)

ggplot(fish_per_year, aes(x = Year, color = Sex)) +
  geom_histogram()

## how many fish per year showing a creek: c creek ratio

ggplot(fish_per_year, aes(x = Year, color = Location)) +
  geom_histogram()


######################################################################
######################################################################

#Looking at return times and distance in each year between each creek#

######################################################################
######################################################################


# A creek fish in *year* 
acreek_fish_2005_return_time <- master_with_distance %>% 
  filter(Year == "2005") %>%    ### Edit year here - then fix name to match year
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "a")

# C creek fish in *year*
ccreek_fish_2010_return_time <- master_with_distance %>% 
  filter(Year == "2010") %>%  ### Edit year here - then fix name to match year
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "c")

## create a scatter plot graph
ggplot(acreek_fish_2005_last_day, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point()    # plot for a creek  - edit name to match year you want
ggplot(ccreek_fish_2010_return_time, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point()    # plot for c creek - edit name to match year you want



#################################################
# How long do fish typically stay in the creek? #
#################################################

fish_creek_stay_length <- master %>% 
  select(Year_Tag, Sex) %>% 
  group_by(Year_Tag) %>% 
  summarise("" = n()) 


ggplot(fish_creek_stay_length, aes(x = count)) +
  geom_histogram() +
  labs(x = "days") +
  scale_x_continuous(limits = c(0, 50))



######################################
# How many fish per section each year#
######################################

location_count_2004_ccreek <- master %>% 
  select(Year, Location, location_section) %>% 
  filter(Year == 2004, Location == "c" ) %>% 
  group_by(location_section) %>% 
  summarise("number_of_fish" = n())

ggplot(location_count_2004_ccreek, aes(x = location_section, y = number_of_fish)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75))



