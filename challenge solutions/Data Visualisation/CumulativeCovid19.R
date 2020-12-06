#Importing Libraries

library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)

#Data preparation

library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


covid_data_2020_cumulative_cases <- covid_data_tbl %>%
  select(countriesAndTerritories, month, year, cases) %>% 
  set_names(c("country", "month", "year", "cases")) %>%
  
  
  filter(year == "2020") %>%
  filter(country %in%  c("Germany", "Spain", "France","United_States_of_America","United_Kingdom"))  %>%
  
  # Group the columns by months
  group_by(month,country) %>%
  summarize(cases = sum(cases)) %>% 
  ungroup() %>%
  group_by(country) %>%
  mutate (cases = cumsum(cases)) %>%
  ungroup() %>%

  mutate(cumulative_cases = scales::dollar(cases, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = ""))

#Ploting the curve
covid_data_2020_cumulative_cases  %>%
  ggplot(aes(x = month, y = cases, color = country)) +

  geom_line(size = 1) +
  
  
  geom_label_repel(aes(x=month, y=cases, label=cumulative_cases) , 
                   data = covid_data_2020_cumulative_cases %>% slice(which.max(cases)),
                    vjust = 0.5, 
                    hjust = 2.5,color = "#08306B")+

  # Formatting
  expand_limits(y = 0) +
 
  scale_x_continuous(breaks = covid_data_2020_cumulative_cases$month,
                     labels = month(covid_data_2020_cumulative_cases$month, label = T)) +
  
  scale_y_continuous(breaks = seq(0e6, 16e6, by = 2e6),
                     labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M")) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country"
  )  +
  
  theme_light() +
  theme(title = element_text(face = "bold", color = "#08306B"),
        legend.position  = "bottom",
        axis.text.x = element_text(angle = 45))

  