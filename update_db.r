library(dplyr)
library(tidyr)
library(lubridate)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
setwd("C:/Users/sjara/git/made-in-america-yarns/data")

# import all data
#--------------------------------------------------------------

# import data as a list
data <- import_data()

# extract data frames from list
order <- as.data.frame(data[[1]])
product <- as.data.frame(data[[2]])
order_main <- as.data.frame(data[[3]])
product_main <- as.data.frame(data[[4]])


# clean new data
#--------------------------------------------------------------

order <- clean_order()
product <- clean_product()

# update main data
#--------------------------------------------------------------

# update matching rows from product_main to match product
# & insert non-matching rows from product to product_main
product_main <- rows_upsert(product_main,product, by="ID")

# insert non-matching ids from order into order_main
order_main <- rows_upsert(order_main, order, by="order_id")


