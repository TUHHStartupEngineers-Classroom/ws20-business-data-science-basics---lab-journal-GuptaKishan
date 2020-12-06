# Importing libraries

library(tidyverse) 
library(rvest)     
library(xopen)    
library(jsonlite)  
library(glue)      
library(stringi)  
library(httr)     

url  <- "https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW"

html <- url %>% 
        read_html()

rank <- html %>% 
        html_nodes(".a-text-right.mojo-header-column.mojo-truncate.mojo-field-type-rank") %>% 
        html_text() %>% 
        as.numeric()


title <-html %>% 
        html_nodes(".mojo-field-type-title > a") %>% 
        html_text() 


link <- html %>%
        html_nodes(".mojo-field-type-title > a") %>% 
        html_attr("href")


release_year <- html %>%
                html_nodes(css = ".a-text-left.mojo-field-type-year") %>%
                html_text() %>%
                enframe(name = "index", value = "release_year") %>%
                slice(2:201)


lifetime_gross <-  html %>%
                   html_nodes(css = ".a-text-right.mojo-field-type-money") %>%
                   html_text() %>%
                   enframe(name = "position", value = "lifetime_gross") %>%
                   slice(2:201)

box_office_mojo_database <- tibble(title,rank,link,release_year,lifetime_gross) %>%

mutate(url = glue("https://www.boxofficemojo.com/{link}")) %>%
  
  select(rank,title,lifetime_gross,url,release_year)

