# Natalie Olson
# Table

# Load packages
library(tidyverse)
library(dplyr)
library(readr)
library("kableExtra")

# Load data set
IMDB_Horror_movies <- read_csv("~/info201/archive/IMDB Horror movies.csv")

# Create dataframe based on key columns
# Grouped by top movie rating in the US
horror_movie_df <- IMDB_Horror_movies %>% 
  select(Title, `Review Rating`, `Release Date`, Budget, `Release Country`) %>% 
  group_by(`Review Rating`) %>% 
  filter(!is.na(`Review Rating`)) %>% 
  filter(!is.na(Budget)) %>% 
  filter(`Release Country` == "USA") %>% 
  relocate(`Review Rating`) %>% 
  arrange(desc(`Review Rating`)) %>% 
  head(n=10)
