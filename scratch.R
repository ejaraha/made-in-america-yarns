library(dplyr)
library(tidyr)
library(lubridate)

# IMPORT DATA
#--------------------------------------------------------------

# set working directory
setwd("C:/Users/sjara/git/made-in-america-yarns/data/raw")

# load keys to select necessary fields
order_fields <- read.csv("order_fields.csv") %>%
  filter(keep==1)
product_fields <- read.csv("product_fields.csv") %>%
  filter(keep==1)

# load raw data then select necessary fields
order <- read.csv("order.csv") %>%
  select(order_fields$variable)
product <- read.csv("product.csv") %>%
  select(product_fields$variable)

# CLEAN order df
#--------------------------------------------------------------



check_empty <- function(df){
  #checks every column in df for NAs and ""
  df <- data.frame("any_nas" = apply(df, 2, function(df) any(is.na(df))),
                   "any_empty_character_vectors" = apply(df, 2, function(df) any(df == "", na.rm=TRUE))) 
  return(df)
}

order <- order %>% 
  mutate(order_id = as.character(order_id),
         order_date = ymd_hms(order$order_date),
         order_hour = hour(order_date),
         order_date = as_date(ymd_hms(order$order_date)),
         customer_id_addition = 1:nrow(order)) %>%
  mutate_if(is.character, list(~na_if(tolower(.),""))) %>% 
  unite("customer_id", c(customer_id, customer_id_addition), sep="_") 

glimpse(order)
check_empty(order)


# CLEAN product df
#--------------------------------------------------------------


product <- product %>%
  mutate(ID = as.character(ID)) %>%
  mutate_if(is.character, list(~na_if(tolower(.),"")))


glimpse(product)
check_empty(product)
















