library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

check_empty <- function(df){
  #checks every column in df for NAs and ""
  df <- data.frame("any_nas" = apply(df, 2, function(df) any(is.na(df))),
                   "any_empty_character_vectors" = apply(df, 2, function(df) any(df == "", na.rm = TRUE))) 
  return(df)
}

# IMPORT DATA
#--------------------------------------------------------------

setwd("C:/Users/sjara/git/made-in-america-yarns/data")

file_list <- list.files()

df_list <- lapply(file_list, 
               FUN = function(f){
                 read.csv(f)
               })

df_names <- str_replace(file_list, ".csv", "")
names(df_list) <- df_names




# # set working directory
# setwd("C:/Users/sjara/git/made-in-america-yarns/data/new")
# 
# # load keys to select necessary fields
# order_cols <- read.csv("order_cols.csv") %>%
#   filter(keep==1)
# product_cols <- read.csv("product_cols.csv") %>%
#   filter(keep==1)
# 
# # load raw data then select necessary fields
# order <- read.csv("order.csv") %>%
#   select(order_cols$variable)
# product <- read.csv("product.csv") %>%
#   select(product_cols$variable)

# CLEAN order_main df
#--------------------------------------------------------------

id_df <- data$order_main %>%
  distinct(billing_email) %>%
  mutate(customer_id=1:nrow(.)) 

data$order_main <- data$order_main %>%
  select(-customer_id) %>%
  left_join(.,id_df, by="billing_email") %>%
  mutate_if(is.character, list(~na_if(tolower(.),""))) %>%
  filter(shipping_country == "us") %>%
  select(-shipping_country)




# CLEAN product_main df
#--------------------------------------------------------------

data$product_main %>% filter(ID == 13654)



# NORMALIZE 
#--------------------------------------------------------------

# MAKE order df
#--------------------------------------------------------------

order <- data$order_main %>%
  mutate(order_id = as.character(order_id),
         order_date = ymd_hms(order$order_date),
         order_hour = hour(order_date),
         order_date = as_date(ymd_hms(order$order_date)))

# 
# write.csv(order, file="C:/Users/sjara/git/made-in-america-yarns/data/live/order.csv", row.names=FALSE)
# glimpse(order)
# check_empty(order)
