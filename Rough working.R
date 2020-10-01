library(tidyverse)
library(readr)


raw_data = read.csv('/Users/Alex/Documents/GitHub/comm2501Ass3/uspollution_pollution_us_2000_2016.csv')


raw_data=separate(data=raw_data, col='Date.Local', into=c('year', 'month', 'day'), sep='-')
x= raw_data %>%
  select(State, City, month, day, year, NO2.Mean, O3.Mean) %>%
  group_by(year, month, State) %>%
  summarise(count=n(), mean(NO2.Mean), mean(O3.Mean))
View(x)

