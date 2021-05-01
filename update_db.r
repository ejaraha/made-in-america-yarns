library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
setwd("C:/Users/sjara/git/made-in-america-yarns/data")


# import all data (_raw.csv, _cols.csv, _main.csv)
#--------------------------------------------------------------
data <- import_data() %>% sort_cols()

# drop unnecessary fields from raw data (_raw_drop.csv)
#--------------------------------------------------------------

## add the _raw_drop files to the data list
data <- append(data, drop_cols())

# update main data with clear, most recent data (_main.csv <- update(_main.csv + _raw_drop.csv))
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


# normalize data
#-----------------------------------------------------------------

data_norm <- create_normalized_dfs(data)

check_primary_keys(define_primary_keys())

lapply(data_norm, check_empty)
