library(dplyr)
library(tidyr)
library(lubridate)

# import data
#--------------------------------------------------------------

import_new_data <- function(order_cols="order_cols.csv",
                        order="order_raw.csv",
                        product_cols="product_cols.csv",
                        product="product_raw.csv"){

  # load order_cols and product_cols 
  # then filter them to create vectors of only the wanted fields
  order_cols <- read.csv(order_cols) %>%
    filter(keep==1) 
  product_cols <- read.csv(product_cols) %>%
    filter(keep==1)
  
  # load order and product dfs 
  # then use order_cols and product_cols to filter their fields
  order <- read.csv(order) %>%
    select(order_cols$variable)
  product <- read.csv(product) %>%
    select(product_cols$variable)
  
  return(list(order, product))
}


# clean data
#--------------------------------------------------------------

clean_order <- function(df=order){
  
  order <- df
  
  order_clean <- order %>% 
    mutate_if(is.character, list(~na_if(tolower(.),"")))
  
  return(order_clean)
  }

clean_product <- function(df=product){
  
  product <- df
  
  product_clean <- product %>%
    mutate_if(is.character, list(~na_if(tolower(.),"")))
  
  return(product_clean)
}


















