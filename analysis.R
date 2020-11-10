#credit to Simon Ivan Chow(TA)
#this is exercise for lab meeting. I hope it can help your A3 and group project!
# Set up <- those are the same libraries you need to install for A3
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

#load data
# Load the *state level* data into a variable. `states` 
states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Load the *county level* data into a variable. `counties`
counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#Goals: creating a ggplot of COVID cases data over time across 4 states 
#and create a chloropleth map (colored map) of cases in counties in WA
states$date = as.Date(states$date)

#Step 1: take states data and filter it down to 4 states (hint: dplyr function)
filtered_states <- states %>% 
  filter(state == "Washington" | state == "Oregon"| state == "California" | state == "New York") 

#Step 2: Create a ggplot of the cases over time in the 4 states
#recommend to exercise 16 - 1

cases_year_plot<- ggplot(filtered_states) +
  geom_point(aes(x = date, y = cases, group = state, color = state)) 

#Step 3: Get most recent counties data from dataset 
counties_most_recent <- counties %>% 
  filter(date == max(date))

#Step 4: Use map_data function to join COVID 'counteies' dataset with the map_data for county
#(remember filter for wahsington)
county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")
map_data <- county_shapes %>% 
  left_join(counties_most_recent, by = "fips") %>% 
  filter(state == "Washington" & county!="Unknown")
#View(county_shapes)
#View(map_data)
#head(county_shapes)

#Step 5: Incorporate blank theme (essentially allows the map to be outputted nicely without
#and weird axes or other such "graph-things" in the way)
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), #remove axis lines
    axis.text = element_blank(), #remove axis labels
    axis.ticks = element_blank(), #remove axis ticks
    axis.title = element_blank(), #remove axis titles
    plot.background = element_blank(), #remove gray background
    panel.grid.major = element_blank(), #remove major grid lines
    panel.grid.minor = element_blank(), #remove minor grid lines
    panel.border =  element_blank() #remove border around plot
  )

# Step 6: Create the map
cases_map <- ggplot(map_data)+
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = cases),
    color = "gray", size = 0.3
  ) +
  coord_map()+
  scale_fill_continuous(limits = c(0, max(map_data$cases)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("COVID Cases in WA")



