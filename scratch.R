library(dplyr)

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



