library(tidyverse)
library(ggthemes)
library(scico)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2020, week = 14)


brewer_size <- tuesdata$brewer_size
beer_states <- tuesdata$beer_states


brewer_size


#First plot
brewer_size %>%
  group_by(brewer_size) %>%
  summarize(avg_bbls = mean(total_barrels, na.rm = TRUE)) %>%
  head(13) %>%
  arrange(desc(avg_bbls)) %>%
  ggplot(aes(avg_bbls, fct_reorder(brewer_size, avg_bbls))) +
  geom_col() +
  theme_economist_white() +
  labs(x = 'Average Barrels Produced', y = 'Brewer Size (Production)',
       caption = '@TidyTuesdayR') +
  ggtitle('Domestic Beer Production by Brewer Size') +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0,160000000))

#Reduction in output year over year for major brewers
brewer_size %>%
  group_by(brewer_size, year) %>%
  summarize(avg_bbls = mean(total_barrels, na.rm = TRUE),
            cum_sum = cumsum(total_barrels)) %>%
  ggplot(aes(avg_bbls, fct_reorder(brewer_size, avg_bbls))) +
  geom_point(aes(color = year), size = 5) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0,160000000)) +
  labs(x = 'Average Production',
       y = 'Brewer Size by Production',
       title = 'Domestic Beer Production by Brewer Size',
       caption = 'Alcohol and Tobacco Tax and Trade Bureau (TTB)') +
  scale_color_scico(palette = "berlin") +
  guides(color = guide_legend()) +
  geom_text(aes(x = 115000000, y = 13,
                label = 'Large decline (2009-2019) in output for major brewers', 
                ))
beer_states
beer_states <- as.Date(beer_states$year)

beer_states %>%
  filter(state!='total') %>%                                    #Remove Total Row from DF
  group_by(state, year) %>%                                     #Groups by state, year
  summarize(sum_bbls = sum(barrels, na.rm = TRUE),              #for groups, sumarize production
            cum_bbls = cumsum(barrels)) %>%                     #for groups, summarize cum production
  arrange(desc(sum_bbls)) %>%                                   #Arrange output by sum_prod large to small
  filter(sum_bbls > 7000000) %>%                                #Filter by prod > 2 MM bbls
  ggplot(aes(year, sum_bbls)) +                                 #Set up ggplot
  geom_line(aes(color = state), show.legend = FALSE) +          #Add line plot, color by state, no legend
  geom_point(aes(color = state), show.legend = FALSE, size = 2)+ #Add points, color, no legend, make em big
  scale_y_continuous(labels = scales::comma,                     #Change y to comma format
                     breaks = seq(5000000,30000000,10000000),        #Set scale window and labels
                     minor_breaks = seq(5000000,30000000,5000000)) +  #set minor lines
  facet_wrap(~ state)+                                                #Facet by State
  labs(x = 'Production Year',                                         #Set chart formats
       y = 'Production (bbl/yr)',
       title = 'Domestic Beer Production by State (2008-2018)',
       subtitle = 'Production > 2,000,000 bbls',
       caption = 'Alcohol and Tobacco Tax and Trade Bureau (TTB)') +
  scale_color_ptol('state') +
  theme_minimal()




