# Natalie Olson
# Table

# Load packages
library(tidyverse)
library(dplyr)
library(readr)
library("kableExtra")

# Load data set
IMDB_Horror_movies <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/IMDBHorrormovies.csv")

# Create dataframe based on key columns
# Grouped by top movie rating in the US
table_horror_movie_df <- IMDB_Horror_movies %>% 
  rename( `Review Rating` = Review.Rating ,`Release Date` = Release.Date, `Release Country` = Release.Country) %>% 
  select(Title, `Review Rating`, `Release Date`, `Release Country`) %>% 
  group_by(`Review Rating`) %>% 
  filter(!is.na(`Review Rating`)) %>% 
  filter(`Release Country` == "USA") %>% 
  relocate(`Review Rating`) %>% 
  arrange(desc(`Review Rating`)) %>% 
  head(n=10)