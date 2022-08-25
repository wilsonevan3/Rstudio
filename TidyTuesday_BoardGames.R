library(tidyverse)
library(tidytuesdayR)
library(stringr)
library(stringi)

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <-tuesdata$ratings
details <- tuesdata$details

ratings <- ratings %>%
        select(num:users_rated)

details

details %>%
  mutate(category = str_replace_all(boardgamecategory, "[^[:word:]]", "")) %>%
  mutate(category1 = str_extract(category, "([:upper:])([:lower:]){1,}")) %>%
  select(category, category1)


         