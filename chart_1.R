# Gulnoor bar graph months on x axis, imbd rating on y, grouped by months. hypothesis - halloween is the best season. 
# insights, summary. 

install.packages("quantmod")
install.packages("zoo")
install.packages("lubridate")
library("quantmod")
library("zoo")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("lubridate")

Horror <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-group4/main/IMDBHorrormovies.csv", stringsAsFactors = TRUE)

Horror <- Horror %>% mutate(Release.Date = dmy(Release.Date))

Horror <- Horror %>% mutate(Release.Date = as.Date(Release.Date, format = "%d/%m/%Y"))

Horror_months <- Horror %>% group_by(month = lubridate::month(Release.Date)) %>%
  summarize(IMBD_Rating = mean(Review.Rating, na.rm = TRUE))

Horror_months <- na.omit(Horror_months)

ggplot(Horror_months)+
  geom_bar(mapping = aes(
    x= month,
    y= IMBD_Rating,
    fill= as.factor(month)), colour = "black",stat = "identity")+
  labs(title = "IMBD rating in different months",x = "Months", y = "IMBD Rating",
       fill = "Months")+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(limits = c(0,5.5))+
  scale_fill_brewer(palette = "Paired")+
  scale_fill_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))+
  theme(plot.title = element_text(hjust = 0.5))
# We thought of this plot because we wanted to check our hypothesis, about the movies 
#being rated best, or the best rated movies being around Halloween. 
# The data confirms that hypothesis, with October being the best month 
#in the year to release a horror movie. This spot is right before Halloween. 
#The worst time would be around January and march. this is also right at the 
#opposite end of the year from Halloween. 
# One could either argue there is a bias around Halloween, or that we generally
# feel more afraid around it. 
  

