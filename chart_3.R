# Caleb's fancy chart correlating a movie's budget and its success in theaters.

library("tidyverse")
library("dplyr")
library("ggplot2")

movie_data <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/IMDBHorrormovies.csv", stringsAsFactors = TRUE)
exchange_rates <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/exchange_rates.csv") %>% 
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

single_decimal_scale <- function(x) sprintf("%.1f", x)

movie_data %>% 
  drop_na() %>%
  ggplot(aes(x = budgets_usd, y = Review.Rating)) + geom_point() + 
  labs(title = "How Does a Production Budget Impact a Movie's Viewer Rating?", 
    x = "Production Budget ($USD)", y = "IMDB Rating") + expand_limits(y = 0) +
  scale_x_continuous(limits = c(0, 1500000), labels = label_number_si()) +
  scale_y_continuous(breaks = seq(0, 10, 1), labels = single_decimal_scale)
