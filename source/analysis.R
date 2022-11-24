library(tidyverse)


#load data
incar_data_whole <-read.csv("../../../data/incarceration_trends.csv")

#create a data frame with just values I intend to look at (prison related)
prison <- incar_data_whole %>% 
  select(year, state, county_name, total_pop, total_pop_15to64,male_pop_15to64,black_pop_15to64, white_pop_15to64, black_male_prison_adm, white_male_prison_adm, total_prison_adm, total_prison_adm_rate, male_prison_adm_rate, black_prison_adm_rate, white_prison_adm_rate)


## Section 2  ---- 
#----------------------------------------------------------------------------#

#What is the average value of my variable across all the counties (in a given year)? 
# *in a given year, what is average number of white male admitted and black male admitted?
average_males <- prison%>% 
  group_by(year) %>% 
  summarise(
    average_black = mean(black_male_prison_adm, na.rm = T),
    average_white = mean(white_male_prison_adm, na.rm = T)
  )

#  Where is my variable the highest or lowest?  
#in which year is the average white the highest/lowest, in which year is the average black highest/lowest 

highest_black <- average_males %>% 
  filter(average_black == max(average_black, na.rm = T)) %>% 
  pull(year)

lowest_black <- average_males %>% 
  filter(average_black == min(average_black, na.rm = T)) %>% 
  pull(year)

highest_white <- average_males %>% 
  filter(average_white == max(average_white, na.rm = T)) %>% 
  pull(year)

lowest_white <- average_males %>% 
  filter(average_white == min(average_white, na.rm = T)) %>% 
  pull(year)

#find total jail adm each year 
total_adm_each_year <- prison %>% 
  group_by(year) %>% 
  summarise(total_prison_adm = sum(total_prison_adm, na.rm = T))

#  How much has my variable change over the last N years?
#Using the average_males, what is the difference between the number of black and white males admitted each year, how does this difference increase/decrease

#find difference between black and white averages
average_males <- average_males %>% 
  mutate(difference_in_averages = average_black - average_white)

#*in which year is the difference between black/white greatest
biggest_difference <- average_males %>% 
  filter(difference_in_averages == max(difference_in_averages, na.rm = T)) %>% 
  pull(year)

smallest_difference <- average_males %>% 
  filter(difference_in_averages == min(difference_in_averages, na.rm = T)) %>% 
  pull(year)

#----------------------------------------------------------------------------#
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function creates the data frame that returns a year and the total prison population 
#I am calculating PRISON NOT JAIL. The chart will not match the provided chart
#despite the provided table and function names representing jail, you are asking for PRISON
#Also the heading refers to prison as well. 

#create function that crease a table with the total prison population for each year 
get_year_jail_pop <- function() {
 make_new_df <- incar_data_whole %>% 
      group_by(year) %>% 
      summarise(total_prison_population_per_year = sum(total_prison_pop, na.rm = T))
return(make_new_df)
}

# This function will plot the table above into a bar chart
#load scales to adjust y axis values 
library(scales)

#create plot function
plot_jail_pop_for_us <- function()  {
  make_table_year_jail_prop <- get_year_jail_pop()
  us_jail_pop_bar_chart <- ggplot(make_table_year_jail_prop, aes(x=year,y=total_prison_population_per_year))+
    geom_bar(stat = "identity")+
    scale_y_continuous(labels = label_comma())+
    labs(y="Total Jail Population",
         x= "Year",
         title = "Increase of Prison Population in the U.S. (1970-2018)", 
         caption = "This bar chart describes the growth of the U.S. prison population from 1970-2018.")
  return(us_jail_pop_bar_chart)
} 

#----------------------------------------------------------------------------#
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
#Similar to section 3, I will be calculating PRISON not jail.
#Despite the variable names, the question asks for prison not jail. 

#create function to generate table that includes the year, state, total prison population 
get_jail_pop_by_states <-function(states) {
  state_prison_growth <- incar_data_whole %>% 
    group_by(year, state) %>% 
#filter by the parameter, where the states presented must match the states you enter
    filter(state %in% states) %>% 
    summarise(state_total_pop = sum(total_prison_pop, na.rm = T)) %>% 
    return(state_prison_growth)
  }
  
#create plot of above table
plot_jail_pop_by_states <- function(states){
  table <- get_jail_pop_by_states(states)
  make_table <- ggplot(table, aes (x =year, y=state_total_pop, color = state)) +
  geom_line() +
  ggtitle("Prison Population for States Over Time")+
  labs(y = "Prison Population", x = "Years", color = "State", caption = "This chart compares the prison population between given states.")
  return(make_table) 
}

#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
#compare the average number of admitted black/white males into prison over time
#----------------------------------------------------------------------------#

#create a table that calculated the average number of black/white males admitted each year
get_averages_df <- function (){
  make_new_df <- prison%>% 
  group_by(year) %>% 
  summarise(
    Average_number_of_black_males = mean(black_male_prison_adm, na.rm = T),
    Average_number_of_white_males = mean(white_male_prison_adm, na.rm = T))%>% 
#to make the data frame suitable for plotting, rearrange with pivot_longer
    pivot_longer(cols = c(Average_number_of_black_males, Average_number_of_white_males), names_to = "race")
  return(make_new_df)
}

#create the for the above table 
averages_plot <- function() {
  use_this_table <- get_averages_df()
  difference_race_chart <- ggplot(use_this_table, aes(fill= race, y= value, x=year)) +
  geom_bar(position = "dodge", stat = "identity", width = .5) +
  labs(fill= "Race", y= "Average Males Admitted", x="Year",
       title = "Average Number of Males Admitted Into Prison by Race", 
       caption = "This chart calculates the average number of Males admitted into prison over time categorized by race.")
  return(difference_race_chart)
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Create a map that reflects the ratio of black to white admitted people into
# prison in the year 2010. This will be a map of the whole country 

#create map
state_shape <- map_data("state")
ggplot(state_shape) + geom_polygon(
  mapping = aes(x = long, y = lat, group = group), color = "white", 
  size = .1 
)+
  coord_map()

#create table to use in function
#calculate the total black/white prison prop in 2010
create_black_white <- function(){
Black_and_white_each_state <-  incar_data_whole %>% 
  filter(year == 2010) %>%  
  group_by(state) %>%  
  summarise(total_black = sum(black_male_prison_adm, na.rm = T),
            total_white = sum(white_male_prison_adm, na.rm = T)) %>% 
#create ratio for each state
  mutate(ratio_black_to_white = total_black/total_white) %>% 
#change the state codes to full state names, adjusting table to match map_data to merge)
  mutate(region = state.name[match(state, state.abb)]) %>% 
  mutate(region = tolower(region)) %>% 
select(region, ratio_black_to_white)
return(Black_and_white_each_state)
}

#join created table (above) with map_data("state")
join_tables <- function(){
Black_and_white_each_state <- create_black_white()
map_state <- map_data("state") %>% 
  left_join(Black_and_white_each_state, by="region")
return(map_state)
}

#create plot with above function(s)
create_plot_map<- function(){
final_table <- join_tables()
#set theme
blank_theme <- theme_bw() + theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  plot.background = element_blank(),
)
create_map <-ggplot(final_table) + geom_polygon(
  mapping = aes(x = long, y = lat, group = group, fill = ratio_black_to_white), color = "white",
  size = .1 
)+
  coord_map() +
  scale_fill_continuous(low = "132B43 ", high = "Red") +
  labs(fill = "Ratio of Black to White Males Admitted", 
       title = "The Ratio of Black to White Males Admitted Into Prison Based on State", 
       caption = "Grey represents uncollected. The largest ratios occur in the east while the least occur in the west. ") +
  blank_theme
return(create_map)
}
