
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

sex_ratio_fish_count <- ggplot(fish_per_year, aes(x = Year, color = Sex)) +
  geom_histogram()

ggsave("figures/sex_ratio_fish_count.jpg")

## how many fish per year showing a creek: c creek ratio

creek_ratio_fish_count <- ggplot(fish_per_year, aes(x = Year, color = Location)) +
  geom_histogram()

ggsave("figures/creek_ratio_fish_count.jpg")

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
ccreek_fish_2005_return_time <- master_with_distance %>% 
  filter(Year == "2005") %>%  ### Edit year here - then fix name to match year
  group_by(Tag) %>% 
  filter(Date == first(Date)) %>% 
  filter(Location == "c")

## create a scatter plot graph
ggplot(acreek_fish_2005_return_time, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75))   # plot for a creek  - edit name to match year you want


ggplot(ccreek_fish_2005_return_time, aes(x = Date, y = midpoint, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75))    # plot for c creek - edit name to match year you want



#################################################
# How long do fish typically stay in the creek? #
#################################################

## looks at all years at once...

fish_creek_stay_length <- master %>% 
  #filter(Year == xxxx) %>%     ### remove hastag if you want to specify a year
  select(Year_Tag, Sex) %>% 
  group_by(Year_Tag) %>% 
  summarise("" = n()) 


creek_stay_length <- ggplot(fish_creek_stay_length, aes(x = count)) +
  geom_histogram() +
  labs(x = "days") +
  scale_x_continuous(limits = c(0, 50))

ggsave("figures/creek_stay_length.jpg")

######################################
# How many fish per section each year#
######################################

## high fish density could mean location of redds and more questions about
## density dependence can be asked

location_count_2004_ccreek <- master %>% 
  select(Year, Location, location_section) %>% 
  filter(Year == 2004, Location == "c" ) %>%    #### Change the year or creek here!!!! Then change the name accordingly
  group_by(location_section) %>% 
  summarise("number_of_fish" = n())

fish_count_per_section_ccreek <- ggplot(location_count_2004_ccreek, aes(x = location_section, y = number_of_fish)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75)) +
  labs( x = "Location Section", y = "Number of fish")

ggsave("figures/fish_count_per_section_ccreek.jpg")


# same graph but opens the scope to showing all years by color


# for c creek
location_count_per_year_ccreek<- master %>% 
  select(Year, Location, location_section) %>% 
  filter(Location == "c" ) %>%    #### Change the year or creek here!!!! Then change the name accordingly
  group_by(location_section, Year) %>% 
  summarise("number_of_fish" = n())

count_per_section_per_year_ccreek <- ggplot(location_count_per_year_ccreek, aes(x = location_section, y = number_of_fish, color = Year)) +
  geom_bar(stat = "identity") +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75)) +
  labs( x = "Location Section", y = "Number of fish")

ggsave("figures/count_per_section_per_year_ccreek.jpg")

### not as helpful or insightful as i thought it'd be... lol

# for a creek
location_count_per_year_acreek<- master %>% 
  select(Year, Location, location_section) %>% 
  filter(Location == "a" ) %>%    #### Change the year or creek here!!!! Then change the name accordingly
  group_by(location_section, Year) %>% 
  summarise("number_of_fish" = n())

count_per_section_per_year_ccreek <- ggplot(location_count_per_year_acreek, aes(x = location_section, y = number_of_fish, color = Year)) +
  geom_bar(stat = "identity") +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust =0.75)) +
  labs( x = "Location Section", y = "Number of fish")

ggsave("figures/count_per_section_per_year_acreek.jpg")



