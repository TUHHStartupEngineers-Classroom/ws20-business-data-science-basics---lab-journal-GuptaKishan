#Importing libraries
library(tidyverse)
library(readxl)

#Importing Files

bikes_tbl      <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")


bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

glimpse(bikes_tbl)

glimpse(orderlines_tbl)

glimpse(bikeshops_tbl)

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

#Wrangling Data 
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  mutate(total.price = price * quantity) %>%
  
  select(-...1, -gender) %>%
  
  select(-ends_with(".id")) %>%
  
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  rename(bikeshop = name) %>%
  
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Data Manipulate

library(lubridate)
sales_by_year_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
  select(order_date, location, total_price, ) %>%
  
  mutate(year = year(order_date)) %>%
  
  separate(col = location,
           into = c("city", "state"),
           sep = ",",
           convert = T) %>%
  
  group_by(state, year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Visualize

sales_by_year_location_tbl %>%
  
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() + 
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and location",
    fill = "Main category" 
  )

