install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)

unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

data_join <- full_join(unicef_indicator_1, unicef_metadata) 
data_join <- full_join(unicef_indicator_1, unicef_metadata, by = "country")
data_join <- full_join(unicef_indicator_1, unicef_metadata, by = c("country"))

map_world <- map_data("world")
filter(data_join, year == "2021")

data_join_2021 <- filter(data_join, year == 2021)
map_data_join <- full_join(data_join_2021, map_world, by = c("country" = "region"))

#INTRODUCTION
#In our journey through the water crisis in schools, let's spotlight the disparities. Graphs and maps will unveil the stark contrast between countries facing the worst conditions and those effectively tackling the challenge. By visualizing these extremes, we aim to shed light on the urgency of addressing this critical issue for the global education landscape.
#map shown Proportion of schools with no drinking water service across the world
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value ) +
  geom_polygon()

# time series chart
ggplot(unicef_metadata) +
  aes(x = year, y = `Life expectancy at birth, total (years)`, color = country) +
  geom_line() +
  guides(color = guide_none())

#This scatter plot illustrates the relationship between a country's GDP per capita (constant 2015 US$) and either the observed value of schools lacking drinking water or life expectancy, depending on the chosen variable. Each point represents a country, color-coded to enhance visibility and differentiate between regions. The plot aims to reveal potential correlations between economic prosperity and critical social indicators, shedding light on broader socio-economic patterns.
#scatter plot
ggplot(data_join, aes(x = `GDP per capita (constant 2015 US$)`, y = obs_value)) +
  geom_point() +
  labs(x = "GDP per Capita (constant 2015 US$)", y = "Observed Value", 
       title = "Scatter Plot of Observed Value vs. GDP per Capita")
ggplot(data_join, aes(x = `GDP per capita (constant 2015 US$)`, y = `Life expectancy at birth, total (years)`)) +
  geom_point() +
  labs(x = "GDP per Capita (constant 2015 US$)", y = "Life Expectancy at Birth (years)", 
       title = "Scatter Plot of Life Expectancy vs. GDP per Capita")


#This bar chart visualizes the top 10 countries with the highest average proportion of schools lacking access to drinking water in 2021. Each bar represents a different country, colored distinctly to facilitate easy comparison. The chart highlights significant disparities in school infrastructure, emphasizing the urgent need for improved water access in these regions.
#Bar Chart
data_2021 <- data_join %>%
  filter(time_period == 2021)
average_data_2021 <- data_2021 %>%
  group_by(country) %>%
  summarise(obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ungroup()
top_countries_2021 <- average_data_2021 %>%
  arrange(desc(obs_value)) %>%
  slice_head(n = 10)
ggplot(top_countries_2021, aes(x = reorder(country, -obs_value), y = obs_value, fill = country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Average Proportion of Schools with No Drinking Water (%)",
       title = "Top 10 Countries Facing School Drinking Water Problems in 2021") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
