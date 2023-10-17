##This Code is for the map data

#The first step is to know and set the working directory

getwd()

#The next step is to install the packages

#The packages to use are tidyverse and maps

#######Tidyverse is already downloaded but not maps

install.packages("maps")

#Load everything

library(tidyverse)
#working with dpylr, tidyr, ggplot, (Placehoder)
library(maps)

#The next step is to upload the tables

whispers_data <- read_csv("event_summary_2023-10-02.csv")

#View it

whispers_data

#Let's clean this up

#First Remove the proprietary columns

whispers_data_clean <- whispers_data %>%
  select(-`Event Type`,-`Public`, -`Countries`)

#Operating in the Event Diagnosis column, replace all text that has
#Batrachochytrium dendrobatidis with Bd then filter out everything that does not
#have a Bd event

whispers_data_cleaner <- whispers_data_clean %>%
  mutate(`Event Diagnosis` = str_replace_all(
    `Event Diagnosis`, "Batrachochytrium dendrobatidis", "Bd")) %>%
  filter(str_detect(`Event Diagnosis`, "Bd"))

#Looks like some states have multiple counties with Bd detection lets separate
#the states and remove the state abbreviations

whispers_data_cleanest <- whispers_data_cleaner %>%
  separate_rows(`Counties (or equivalent)`, sep = ";") %>% #Create a new row based on the separator value";"
  mutate(`Counties (or equivalent)` = str_trim(`Counties (or equivalent)`)) %>% #This removes residual white spaces after the split
  mutate(`Counties (or equivalent)` = str_replace(`Counties (or equivalent)`, " County, [A-Z]{2}", "")) %>% #This replaces with a blank
  mutate(`Counties (or equivalent)` = tolower(`Counties (or equivalent)`))
  

#We now have a clean data frame to work with now it's time to load up a map of the US using Maps

us_map <- map_data("usa")

#Lets visualize the map

USA <- ggplot(data = us_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(fill ="white", color = "black") +
  coord_fixed(1.3) + #for scaling reasons
  theme_minimal() +
  labs(title = "Map of the US")

USA

#This looks good but now lets add the state borders

us_state_map <- map_data("state")

USA_States <- ggplot(data = us_state_map,
                     aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Map of the US with State Border")

USA_States

#The now let's see all of the counties

us_states_counties <- map_data("county")

USA_Counties <- ggplot() +
  geom_polygon(data = us_states_counties,
               aes(x = long, y = lat, group = group),
               fill = "white", color = "gray80") +
  geom_polygon(data = us_state_map,#overlaying the state map with county map
               aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Map of the US with State and County Borders")

USA_Counties

#Lets combine everything


highlight_states_counties <- us_states_counties %>%
  mutate(highlight = if_else(subregion %in% whispers_data_cleanest$'Counties (or equivalent)', TRUE, FALSE))

#The if_else function creates the highlight column which returns TRUE or FALSE
#based on the %in% operator.The %in% operator checks for an element within a
#dataframe. i.e. checking for specific county name. If the county name in the
#WHISPers dataframe matches the county map dataframe, TRUE will be the value in
#hightlight, else it will return FALSE.

map_hightlighted <- ggplot() +
  geom_polygon(data = highlight_states_counties, 
               aes(x = long, y = lat, group = group, fill = highlight),
               color = "gray80") +
  geom_polygon(data = us_state_map, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") + #NA for fill because it will overwrite the highlight color
  scale_fill_manual(values = c("white", "green"), guide = "none") +  
  #I don't know why this specific order but vice versa makes this look bad

  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Batrachochytrium Dendrobatidis Detected from 1999 - 2023")

#As you can see all regions highlighted in Green are from 1999 to 2023
map_hightlighted


#Create timelapse of the spread of Bd from 1999 to 2023

#Anything else can be added after this basic template is followed

#The next step is to add an image of Ur. sapphirina and Ur. lowii in each of the states... On another program
