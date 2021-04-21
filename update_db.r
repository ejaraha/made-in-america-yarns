library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
setwd("C:/Users/sjara/git/made-in-america-yarns/data")


# import all data
#--------------------------------------------------------------
data <- import_data()


# filter raw data
#--------------------------------------------------------------
## order
fields_to_keep <- data$order_cols %>%
  filter(keep == 1)
order <- data$order_raw %>%
  select(fields_to_keep$variable)
## product
fields_to_keep <- data$product_cols %>%
  filter(keep == 1)
product <- data$product_raw %>%
  select(fields_to_keep$variable)


# update main data with filtered data
#--------------------------------------------------------------
## update matching rows from product_main to match product
## & insert non-matching rows from product to product_main
data$product_main <- rows_upsert(data$product_main,product, by="ID")

## insert non-matching ids from order into order_main
data$order_main <- rows_upsert(data$order_main, order, by="order_id")


# clean main data
#--------------------------------------------------------------
data$order_main <- clean_order_main()

glimpse(data$order_main)


glimpse(data$product_main)



