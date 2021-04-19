library(dplyr)
library(tidyr)
library(lubridate)

source("C:/Users/sjara/git/made-in-america-yarns/clean.R")
#source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
wd <- "C:/Users/sjara/git/made-in-america-yarns/data"

order_main <- read.csv("order_main.csv")
product_main <- read.csv("product_main.csv")

# update matching rows from product_main to match product
# & insert non-matching rows from product to product_main
product_main <- rows_upsert(product_main,product, by="ID")

# insert non-matching ids from order into order_main
order_main <- rows_insert(order_main, order, by="order_id")


