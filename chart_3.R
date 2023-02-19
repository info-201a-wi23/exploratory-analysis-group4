# Caleb's fancy chart correlating a movie's budget and its success in theaters.
# The scatter plot below represents ___.
# Note that the exchange rates are those of 31/12/2022 and are subject to fluctuation.

library("tidyverse")
library("dplyr")
library("ggplot2")

movie_data <- read.csv("~/IMDB_Horror_Movies.csv", stringsAsFactors = TRUE)
exchange_rates <- read.csv("~/exchange_rates.csv") %>% 
  group_by(currency) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  select(currency, value)

movie_data <- movie_data %>%
  mutate(currency = 
           ifelse(substr(gsub("[[:space:]]", "", Budget), 0, 1) == "$", paste0("USD"), 
           ifelse(substr(gsub("[[:space:]]", "", Budget), 0, 1) == "£", paste0("GBP"),
           ifelse(substr(gsub("[[:space:]]", "", Budget), 0, 1) == "€", paste0("EUR"),
           substr(gsub("[[:space:]]", "", Budget), 0, 3)))))

movie_data <- left_join(movie_data, exchange_rates, by = "currency")
movie_data <- movie_data %>% 
  mutate(budgets_numeric = as.numeric(gsub("[^[:digit:]. ]", "", Budget)))
movie_data <- movie_data %>% 
  mutate(budgets_usd = trunc(budgets_numeric / value * 1.072673))

# graph the thing? should be simple enough
ggplot(movie_data, aes(x = budgets_usd, y = Review.Rating)) + geom_point() + 
  xlim(0, 1000000) + labs(x = "Production Budget ($USD)", y = "User Rating")

