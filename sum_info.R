# Natalie Olson
# Sum info file

library(dplyr)
library(tidyr)

# Load Dataframe
movie_data <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/IMDBHorrormovies.csv")

# A function that takes in a dataset and returns a list of info about it:
summary_info <- list()

summary_info$num_titles <- nrow(movie_data)

# Pull the highest rating for a movie
summary_info$highest_rating <- movie_data %>% 
  filter(Review.Rating == max(Review.Rating, na.rm = TRUE)) %>% 
pull(Review.Rating)

# Pull the title of the highest rated movie
summary_info$highest_rated_movie <- movie_data %>% 
  filter(Review.Rating == max(Review.Rating, na.rm = TRUE)) %>% 
  pull(Title)

# Pull the lowest rating for a movie
summary_info$lowest_rating <- movie_data %>% 
  filter(Review.Rating == min(Review.Rating, na.rm = TRUE)) %>% 
pull(Review.Rating)
summary_info$lowest_rating <- unique(summary_info$lowest_rating)

# Pull the movie with the lowest rating
summary_info$lowest_rated_movie <- movie_data %>% 
  filter(Review.Rating == min(Review.Rating, na.rm = TRUE)) %>% 
  pull(Title)

# Find the highest rated movie in each country
country_highest_ratings <- movie_data %>% 
  select(Title, Release.Country, Review.Rating) %>% 
  group_by(Release.Country) %>% 
  filter(Review.Rating == max(Review.Rating, na.rm = TRUE)) %>% 
  arrange(desc(Review.Rating))
country_highest_ratings <- unique(country_highest_ratings)

# Pull the highest rated movie in the USA
summary_info$usa_highest_rated <- movie_data %>% 
  select(Title, Release.Country, Review.Rating) %>% 
  filter(Release.Country == "USA") %>% 
  filter(Review.Rating == max(Review.Rating, na.rm = TRUE)) %>% 
  pull(Title)

# Find the average movie rating in each country
country_avg_rating <- movie_data %>% 
  select(Title, Release.Country, Review.Rating) %>% 
  group_by(Release.Country) %>% 
  summarize(avg_rating_per_country = mean(Review.Rating, na.rm = TRUE))

# Find the country with the highest average rating across all genres
summary_info$country_highest_avg_rating <- country_avg_rating %>% 
  filter(avg_rating_per_country == max(avg_rating_per_country, na.rm = TRUE)) %>% 
  pull(Release.Country)

# Separate multiple genres per movie
movie_data_sep_genre <- movie_data %>% 
  separate_rows(Genres, sep = "\\|") %>% 
  filter(!(Review.Rating == ""))

# Find the average rating per genre/sub-genre
movie_data_genres <- movie_data_sep_genre %>%
  select(Title, Genres, Review.Rating) %>% 
  group_by(Genres) %>% 
  summarize(avg_genre_rating = mean(Review.Rating, na.rm = TRUE))

# Find the highest rated genre/sub-genre on average
summary_info$highest_rated_genre <- movie_data_genres %>% 
  filter(avg_genre_rating == max(avg_genre_rating, na.rm = TRUE)) %>% 
  pull(Genres)