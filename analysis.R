# Matthew Kyle Pedraja
# Analysis.R
# Assignment 3: Incarcerations

# load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lintr)
library(maps)
library(mapproj)

Incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Summary Statistics 

# which state had the highest prison population rate in the South in 1990?
state_highest_prison_population_rate_1990 <- Incarcerations %>%
  filter(year == 1990)%>%
  filter(region == "South") %>%
  group_by(region)%>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate, na.rm = TRUE)) %>%
  pull(state)

state_highest_population_rate_number <- Incarcerations %>%
  filter(year == 1990)%>%
  filter(region == "South") %>%
  group_by(region)%>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate, na.rm = TRUE)) %>%
  pull(total_prison_pop_rate)

# which race had the highest jail population rates on average in the South?

aapi_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(aapi_jail_pop_average = mean(aapi_jail_pop_rate, na.rm = TRUE))%>%
  pull(aapi_jail_pop_average)

black_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(black_jail_pop_average = mean(black_jail_pop_rate, na.rm = TRUE))%>%
  pull(black_jail_pop_average)

latinx_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(latinx_jail_pop_average = mean(latinx_jail_pop_rate, na.rm = TRUE))%>%
  pull(latinx_jail_pop_average)


native_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(native_jail_pop_average = mean(native_jail_pop_rate, na.rm = TRUE))%>%
  pull(native_jail_pop_average)


white_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(white_jail_pop_average = mean(white_jail_pop_rate, na.rm = TRUE))%>%
  pull(white_jail_pop_average)

highest_jail_pop_mean <- Incarcerations %>%
  filter (region == "South")%>%
  summarize(aapi_jail_pop_mean, black_jail_pop_mean, latinx_jail_pop_mean, 
            native_jail_pop_mean, white_jail_pop_mean) %>%
  gather(key = "race", value = "jail_pop_rate") %>%
  filter(jail_pop_rate == max(jail_pop_rate)) %>%
  pull(jail_pop_rate)

# which race had the lowest jail population rate on average in the South?
lowest_jail_pop_mean <- Incarcerations %>%
  filter(region == "South")%>%
  summarize(aapi_jail_pop_mean, black_jail_pop_mean, latinx_jail_pop_mean, 
            native_jail_pop_mean, white_jail_pop_mean) %>%
  gather(key = "race", value = "jail_pop_rate_min") %>%
  filter(jail_pop_rate_min == min(jail_pop_rate_min)) %>%
  pull(jail_pop_rate_min)

# which race had the highest prison population rate on average in the South?

aapi_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(aapi_prison_pop_average = mean(aapi_prison_pop_rate, na.rm = TRUE))%>%
  pull(aapi_prison_pop_average)

black_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(black_prison_pop_average = mean(black_prison_pop_rate, na.rm = TRUE))%>%
  pull(black_prison_pop_average)

latinx_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(latinx_prison_pop_average = mean(latinx_prison_pop_rate, na.rm = TRUE))%>%
  pull(latinx_prison_pop_average)


native_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(native_prison_pop_average = mean(native_prison_pop_rate, na.rm = TRUE))%>%
  pull(native_prison_pop_average)


white_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(white_prison_pop_average = mean(white_prison_pop_rate, na.rm = TRUE))%>%
  pull(white_prison_pop_average)

highest_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(aapi_prison_pop_mean, black_prison_pop_mean, latinx_prison_pop_mean, 
            native_prison_pop_mean, white_prison_pop_mean) %>%
  gather(key = "race", value = "prison_pop_rate_highest") %>%
  filter(prison_pop_rate_highest == max(prison_pop_rate_highest)) %>%
  pull(prison_pop_rate_highest)

# which race had the lowest prison population rate on average in the South?
lowest_prison_pop_mean <- Incarcerations %>%
  filter(region == "South") %>%
  summarize(aapi_prison_pop_mean, black_prison_pop_mean, latinx_prison_pop_mean, 
            native_prison_pop_mean, white_prison_pop_mean) %>%
  gather(key = "race", value = "prison_pop_rate_lowest") %>%
  filter(prison_pop_rate_lowest == min(prison_pop_rate_lowest)) %>%
  pull(prison_pop_rate_lowest)
  
## Trends over Time Chart

# What are the prison population rates in Tennessee for black, white, latinx, and native
# people over time from the 1990s to the present?
pop_prison_time_chart <- Incarcerations %>%
  filter(state == "TN")%>%
  filter(year >= "1990")%>%
  group_by(year) %>%
  summarize(black_prison_pop_rate = sum(black_prison_pop_rate, na.rm = TRUE),
            white_prison_pop_rate = sum(white_prison_pop_rate, na.rm = TRUE),
            latinx_prison_pop_rate = sum(latinx_prison_pop_rate, na.rm = TRUE),
            native_prison_pop_rate = sum(native_prison_pop_rate, na.rm = TRUE)) %>%
  gather(key = "race", value = "prison_chart", -year) %>%
  filter(prison_chart > 0)

line_chart <- pop_prison_time_chart %>%
  ggplot(aes(x = year, y = prison_chart, color = race)) + 
  geom_line(alpha = 1.0, size = 1.0) + labs(title = "Prison Population Rates over Time in Tennessee", x = "Year", 
       y = "Prison Population Rates", color = "Race") +
  scale_color_manual(labels = c("Black", "White", "Latinx", "Native"), values = c("pink", "red", "orange", "purple"))
# print(line_chart)

## Variable Comparison Chart
# What is ratio of black people in jail vs black people in prison in Tennessee starting from 1990?

black_jail_prison_ratio <- Incarcerations %>%
  filter(state == "TN") %>%
  filter(year >= 1990)%>%
  group_by(year) %>%
  summarize(black_prison_pop_rate = mean(black_prison_pop_rate, na.rm = TRUE),
            black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE)) 

scatterplot <- black_jail_prison_ratio %>%
  ggplot(aes(x = black_prison_pop_rate, y = black_jail_pop_rate)) +
  geom_point(alpha = 1.5, size = 1.5) + 
  labs(title = "Prison vs Jail Population Rates of Black People in Tennessee",
       x = "Jail Population Rates", y = "Prison Population Rates")

# print(scatterplot)
  
## Map Chart

# What is each counties highest black jail population rate in the South?
  
black_jail_pop_state <- Incarcerations %>%
  filter(region == "South")%>%
  group_by(state, county_name, fips)%>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = TRUE))%>%
  select(black_jail_pop_rate, region)

region_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ',') %>%
  left_join(county.fips, by = 'polyname')

map_data <- region_shape %>%
  left_join(black_jail_pop_state, by = 'fips') %>%
  filter(region == "South")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),       
    axis.text = element_blank(),        
    axis.ticks = element_blank(),      
    axis.title = element_blank(),      
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()      
  )
map_chart <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "black",
    size = 0.5  
  ) +
  coord_map() + 
  scale_fill_continuous(na.value = "white", low = "yellow", high = "red") +
  labs(title = "Highest Black Jail Population Rate In Each County in the South",
        fill = "Jail Population Rates") + blank_theme
# print(map_chart) 


  

