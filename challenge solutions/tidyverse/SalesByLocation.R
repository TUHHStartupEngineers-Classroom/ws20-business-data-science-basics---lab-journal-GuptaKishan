#Importing libraries
library(tidyverse)
library(readxl)

#Importing files
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

#Data wrangling
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

sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
  select(location, total_price) %>%
  
separate(col = location,
         into = c("city", "state"),
         sep = ",",
         convert = T) %>%
  
  group_by(state) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


# Data Visualization

sales_by_location_tbl %>%
  
  ggplot(aes(x = state, y = sales)) +
  
  geom_col(fill = "#4caf50",width = .45) +
  
  geom_text(aes(label = sales_text), size = 2,vjust = -1, hjust = 0.5) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  expand_limits(y = 2.5e7)+
  
  labs(
    title    = "Revenue by location",
    x = "", 
    y = "Revenue"
  )