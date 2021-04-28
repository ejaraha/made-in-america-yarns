library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
setwd("C:/Users/sjara/git/made-in-america-yarns/data")


# import all data
#--------------------------------------------------------------
data <- import_data() %>% sort_cols()

# drop unnecessary fields from raw data
#--------------------------------------------------------------

## add the _raw_drop files to the data list
data <- append(data, drop_cols())


# update main data with  data
#--------------------------------------------------------------
## update matching rows from order_main to match order
## & insert non-matching ids from order_raw_drop into order_main
data$order_main <- rows_upsert(data$order_main, data$order_raw_drop, by="order_id")

## update matching rows from product_main to match product
## & insert non-matching rows from product_raw_drop to product_main
data$product_main <- rows_upsert(data$product_main,data$product_raw_drop, by="id")

## update matching rows from role_main to match role
## & insert non-matching rows from role_raw_drop to role_main
data$role_main <- rows_upsert(data$role_main, data$role_raw_drop, by="id")


# clean main data
#--------------------------------------------------------------
data$order_main <- clean_order_main()
glimpse(data$order_main)

# product data is clean
glimpse(data$product_main)

# raw data is clean
glimpse(data$role_main)
