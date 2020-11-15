library(tidyverse)
library(readr)
library(ggplot2)

# Import the dataset
raw_data = read.csv('/Users/Alex/Documents/GitHub/comm2501Ass3/uspollution_pollution_us_2000_2016.csv')

# Average out the air pollutant data by date
attach(raw_data)
mean_data = raw_data %>%
  group_by(State, Date.Local) %>%
  summarise(no2=mean(NO2.Mean), no2_aqi=mean(NO2.AQI), o3=mean(O3.Mean), 
            o3_aqi=mean(O3.AQI), so2=mean(SO2.Mean), co=mean(CO.Mean), 
            so2_aqi=mean(SO2.AQI, na.rm=TRUE), co_aqi=mean(CO.AQI, na.rm=TRUE), 
            .groups='keep')

# Separate the date out into day, month and year
sep_mean_data = mean_data %>%
  separate(col='Date.Local', into=c('year', 'month', 'day'), sep='-')
sep_mean_data$year_month = paste(sep_mean_data$year, sep_mean_data$month, sep='-')

# Summarisze data set by the month
country_data = sep_mean_data %>%
  filter(year != 2016) %>% # Filter out 2016 (incomplete data set)
  group_by(year_month) %>% 
  summarise(no2=mean(no2), no2_aqi=mean(no2_aqi), so2=mean(so2), 
            so2_aqi=mean(so2_aqi), o3=mean(o3), o3_aqi=mean(o3_aqi), co=mean(co), 
            co_aqi=mean(co_aqi, na.rm=TRUE))

# Reorganise data set so that air pollutant type is a property, not a column
no2 = data.frame(country_data$year_month, emissions=country_data$no2, 
                 aqi=country_data$no2_aqi, type='NO2')
so2 = data.frame(country_data$year_month, emissions=country_data$so2, 
                 aqi=country_data$so2_aqi, type='SO2')
o3 = data.frame(country_data$year_month, emissions=country_data$o3, 
                aqi=country_data$o3_aqi, type='O3')
co = data.frame(country_data$year_month, emissions=country_data$co, 
                aqi=country_data$co_aqi, type='CO')
country_data = bind_rows(no2,so2,o3,co)

# Create ggplot of pollutants over time
attach(mean_data)
years = paste('20', formatC(seq(0,15,1), width=2, flag='0'), '-01', sep='')
ggplot(country_data, mapping=aes(country_data.year_month, aqi, group=type, 
                                 color=type))+
  geom_smooth()+
  geom_line(alpha=0.7)+
  ggtitle('Air Quality Index (AQI) of Common Pollutants in the US')+ 
  labs(y='AQI', x='Year', color='Pollutant')+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(breaks=years)



