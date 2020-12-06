#Importing libraries

library(tidyverse) 
library(rvest)     
library(xopen)    
library(jsonlite)
library(glue)      
library(stringi)   
library(httr)      

url_home          <- "https://www.rosebikes.com/"

html_home         <- read_html(url_home)

bike_category_tbl <- html_home %>%
   
  html_nodes(css = ".main-navigation-category-with-tiles__item > a") %>%
  html_attr('href') %>%
  
  discard(.p = ~stringr::str_detect(.x,"sale")) %>%
  
  # Getting tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  
  # Adding the domain
  mutate(
    url = glue("https://www.rosebikes.com{subdirectory}")
  ) %>%
  
  # WGetting unique values
  distinct(url)
  
bike_category_url <- bike_category_tbl$url[1]

html_bike_category  <- read_html(bike_category_url)

bike_url_tbl        <- html_bike_category %>%
  
  # Select nodes by the ids
  html_nodes(css = ".row.align-middle > a") %>%
  
  html_attr('href') %>%
  
  enframe(name = "position1", value = "sub") %>%

mutate(
  url = glue("https://www.rosebikes.com{sub}")) %>%
  
  distinct(url)

bike_desc_tbl <- html_bike_category %>%
   
  html_nodes(".catalog-category-bikes__subtitle") %>%
  
   html_text() %>%
  
  # Getting tibble
  enframe(name = "position2", value = "description")

bike_name_tbl <- html_bike_category %>%
  
    html_nodes(".catalog-category-bikes__title > span") %>%
  
   html_text() %>%
  
   enframe(name = "position3", value = "name")
 
bike_price_tbl <- html_bike_category %>%
  
  html_nodes(".catalog-category-bikes__price") %>%
  
  html_text() %>%
  
  stringr::str_extract("(?=)[:alpha:]+.*" ) 

  enframe(name = "position4", value = "price")

bike_new <- tibble(bike_url_tbl , bike_price_tbl, bike_name_tbl , bike_desc_tbl) %>%
  select(name,description,price,url)




