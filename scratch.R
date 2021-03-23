library(dplyr)

# set working directory
setwd("C:/Users/sjara/git/made-in-america-yarns/data/raw")


# load keys to select necessary fields
coupon_keep <- read.csv("coupon_keep_fields.csv") %>%
  filter(keep==1)
order_keep <- read.csv("order_keep_fields.csv") %>%
  filter(keep==1)
product_keep <- read.csv("product_keep_fields.csv") %>%
  filter(keep==1)

# load raw data then select necessary fields
coupon <- read.csv("coupon.csv") %>%
  select(coupon_keep$variable)
order <- read.csv("order.csv") %>%
  select(order_keep$variable)
product <- read.csv("product.csv") %>%
  select(product_keep$variable)

glimpse(order)
glimpse(product_keep)

