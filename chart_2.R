# Kazuma's Map Chart
# Make a world map with each country being colored
# Each color will differ depending on the country's average rating (higher rating = darker color of a shade)
# Write purpose and insight by sat night
library(tidyverse)
library(maps)
library(dplyr)
library(ggplot2)

#Load the horror movie data from user file. 
horror_movie_df <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/IMDBHorrormovies.csv",
                            stringsAsFactors = T)
View(horror_movie_df)

#Select only the country and the rating of each number from the orignial data frame.
#drop all the NA of the data frame since we want to ignore those. 
horror_movie_df <- horror_movie_df %>% 
  select(Release.Country, Review.Rating) %>% 
  drop_na(Review.Rating)

#create a temporary data frame that combines all the countries and average the each movie rating from 
#that country
new_df1 <- aggregate(horror_movie_df$Review.Rating, 
                    list(horror_movie_df$Release.Country), 
                    FUN = "mean")
colnames(new_df1) <- c("Country", "Average_Rating")

#set temp dataframe as the horror movie data fram. 
horror_movie_df <- new_df1

# # Create a world map
world_map <- map_data("world")
# 
# # Merge the map data with the horror movie df. 
View(merged_data)
merged_data <- left_join(world_map, horror_movie_df, by = c("region" = "Country"))
# 
# # Create a ggplot object and use geom_map to plot the data
ggplot() +
  geom_map(data = merged_data, map = merged_data, aes(x = long, y = lat, map_id = region, group = group, fill = Average_Rating), color = "white") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradient(low = "white", high = "red", name = "Average Rating") +
  labs(x = "", y = "", title = "Average Horror Movie Rating by Country") +
  theme_void() 
