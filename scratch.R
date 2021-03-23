library(dplyr)

setwd("C:/Users/sjara/git/made-in-america-yarns/data/raw")
coupon <- read.csv("coupon.csv")
order <- read.csv("order.csv")
product <- read.csv("product.csv")

glimpse(coupon)
