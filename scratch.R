library(dplyr)
library(tidyr)
library(lubridate)

# IMPORT DATA
#--------------------------------------------------------------

# set working directory
setwd("C:/Users/sjara/git/made-in-america-yarns/data/new")

# load keys to select necessary fields
order_cols <- read.csv("order_cols.csv") %>%
  filter(keep==1)
product_cols <- read.csv("product_cols.csv") %>%
  filter(keep==1)

# load raw data then select necessary fields
order <- read.csv("order.csv") %>%
  select(order_cols$variable)
product <- read.csv("product.csv") %>%
  select(product_cols$variable)

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

write.csv(order, file="C:/Users/sjara/git/made-in-america-yarns/data/live/order.csv", row.names=FALSE)
glimpse(order)
check_empty(order)


# CLEAN product df
#--------------------------------------------------------------


product <- product %>%
  mutate(ID = as.character(ID)) %>%
  mutate_if(is.character, list(~na_if(tolower(.),"")))

write.csv(product, file="C:/Users/sjara/git/made-in-america-yarns/data/live/product.csv", row.names=FALSE)
glimpse(product)
check_empty(product)
















