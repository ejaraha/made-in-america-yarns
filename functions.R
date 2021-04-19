library(dplyr)
library(tidyr)
library(lubridate)

# import all data
#--------------------------------------------------------------

import_data <- function(order_cols="order_cols.csv",
                        product_cols="product_cols.csv",
                        order="order_raw.csv",
                        product="product_raw.csv",
                        order_main="order_main.csv",
                        product_main="product_main.csv"){

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
  
  # load order_main and product_main
  order_main <- read.csv(order_main)
  product_main <- read.csv(product_main)
  
  return(list(order, product, order_main, product_main))
}


# clean new data
#--------------------------------------------------------------

clean_order <- function(df=order){
  # initialize order df
  order <- df
  # insert NAs, CAP -> low
  order_clean <- order %>% 
    mutate_if(is.character, list(~na_if(tolower(.),"")))
  
  return(order_clean)
  }

clean_product <- function(df=product){
  #initialize product df
  product <- df
  # insert NAs, CAP -> low
  product_clean <- product %>%
    mutate_if(is.character, list(~na_if(tolower(.),"")))
  
  return(product_clean)
}


















