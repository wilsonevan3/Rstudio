library(tidyverse)
library(tidytuesdayR)
library(stringr)
library(stringi)
library(plotly)
theme_set(theme_light())


#Load Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <-tuesdata$ratings
details <- tuesdata$details

theme = light

#Strip weirdness out of the Category and Designer ones. 
details <- details %>%
  mutate(FirstCategory = str_replace_all(boardgamecategory, "[^[:word:]]", "")) %>%
  mutate(FirstCategory = str_extract(FirstCategory, "([:upper:])([:lower:]){1,}")) %>%
  mutate(FirstDesigner = str_replace_all(boardgamedesigner, "([:punct:])", "")) %>%
  select(id, 
         primary, 
         yearpublished, 
         minplayers, 
         maxplayers, 
         playingtime, 
         minplaytime, 
         maxplaytime, 
         FirstCategory, 
         FirstDesigner)

#Reduce size of ratings table
ratings <- ratings %>%
  select(num:users_rated)


#Join into one rectangular dataset
details_ratings <- merge(details, ratings, by = 'id') %>%
  as_tibble(details_ratings) %>%
  select(-name) %>%
  select(id,                #This reorders the columns to put rank and average
         primary,           # as part of the primary tibble when details_ratings
         yearpublished,     # is called, this gives you immediate visability 
         rank,              # of the most interesting columns, and pushes the others
         average,           # back in the line. 
         FirstCategory,
         FirstDesigner,
         minplayers,
         maxplayers,
         playingtime,
         minplaytime, 
         maxplaytime,
         users_rated) %>%
  filter(yearpublished >= 400)
details_ratings

hot_cold <- colorRampPalette(c("red", "green"))
hot_cold_colorbar <- hot_cold(20)  

details_ratings

#Count features and histograms - where is the density of the dataset
details_ratings %>%
  filter(yearpublished >1900) %>%
  group_by(yearpublished) %>%
  summarise(mean_rating = mean(average),
            mean_rank = mean(rank),
            n=n()) %>%
  ggplot(aes(yearpublished, n)) +
  geom_area() +
  labs(title = 'Frequency Distribution of Board Games Published',
       subtitle = '1900-2020',
       caption = 'Data from: BoardGameGeek',
       x = 'Year Published',
       y = '')

#Top 10 categories by average game rating, what types of games do people love?
details_ratings %>%
  group_by(FirstCategory) %>%
  summarize(average = mean(average),
            Count = n()) %>%
  head(10) %>%
  ggplot(aes(fct_reorder(FirstCategory, average), average, label = round(average,1))) +
  geom_point(aes(size = Count, color = average)) +
  labs(title = 'Average Rating by Category',
       caption = 'Ratings from BoardGameGeek.com',
       color = 'Average User Rating',
       size = 'Number of Games',
       x = '',
       y = 'Average Rating') +
  geom_text(vjust = 1.7,
            position = position_dodge(0.9))
  













         