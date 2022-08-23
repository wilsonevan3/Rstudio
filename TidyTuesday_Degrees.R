phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

library(plotly)
library(tidyverse)
library(shiny)

degrees <- phd_field

degrees %>%
  plot_ly(x = ~n_phds,
          y = ~year,
          color = ~broad_field) %>%
  layout(title = 'Number of US Doctoral Degrees by Area 2008-2017',
         xaxis = list(title = 'Number of Ph.D Degrees'),
         yaxis = list(title = 'Year'))


#Bar Graph of all the degree fields
degrees %>%
  group_by(broad_field, field, year, n_phds) %>%
  plot_ly(x = ~broad_field,
          y = ~n_phds,
          color = ~factor(year),
          text = ~field)

#Growth and Decline of degree fields by Broad_Field
fig <- degrees %>%
  filter(n_phds > 50) %>%
  group_by(field)%>%
  arrange(field, year) %>%
  mutate(rate = 100*(n_phds-lag(n_phds))/lag(n_phds)) %>%
  arrange(desc(rate)) %>%
  group_by(field) %>%
  mutate(CumRate = round(cumsum(rate), digits = 1)) %>%
  filter(!is.na(CumRate), year == 2017) %>%
  arrange(major_field) %>%
  plot_ly(x = ~CumRate,
          y = ~major_field,
          text = ~field,
          size = ~n_phds,
          type = 'scatter', 
          color = ~broad_field)%>%
  layout(showlegend = FALSE,
         title = 'Cumulative Rate of Growth in Ph.D Candidates by Field',
         xaxis = list(
           title = 'Cumulative Growth/Decline 2008-2017'),
         yaxis = list(
           title = '')
         )
fig

degrees %>%
  filter(n_phds > 50) %>%
  group_by(field)%>%
  arrange(field, year) %>%
  mutate(rate = 100*(n_phds-lag(n_phds))/lag(n_phds)) %>%
  arrange(desc(rate)) %>%
  group_by(field) %>%
  mutate(CumRate = round(cumsum(rate), digits = 1)) %>%
  filter(!is.na(CumRate), year == 2017) %>%
  arrange(desc(CumRate)) %>%
  head(10) %>%
  plot_ly(y = ~CumRate,
          x = ~fct_reorder(field, CumRate),
          size = ~n_phds,
          color = ~broad_field,
          type = 'bar',
          text = ~CumRate,
          textposition = 'auto') %>%
  layout(title = 'US Ph.D Growth 2008-2017',
         subtitle = 'Top 10 Highest Growth Areas',
         xaxis = list(title = ''),
         yaxis = list(title = '% Growth 2008-2017'))


degrees %>%
  filter(!is.na(n_phds)) %>%
  group_by(broad_field, year) %>%
  summarize(CumSum = sum(n_phds))%>%
  ggplot(aes(year, fill = broad_field)) +
  geom_area(aes(y = CumSum), position = position_dodge(), alpha = 0.2) +
  geom_point(aes(y =CumSum))





