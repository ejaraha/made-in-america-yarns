library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

setwd("C:/Users/sjara/git/made-in-america-yarns/data")
source("C:/Users/sjara/git/made-in-america-yarns/functions.R")

data_denorm <- read.csv("denormalized.csv", stringsAsFactors = FALSE)

glimpse(data_norm$order_product)

glimpse(data_denorm)


top_products_by_ym <- data_denorm %>%
  # get order-level data and all necessary fields
  distinct(order_id, product_id, order_ym, name, quantity) %>% 
  # get product-level info
  group_by(order_ym, product_id) %>% 
  # most orders placed by year/mo
  summarize(n_orders = n(),
  # largest quantity purchased
         sum_quantity = sum(quantity),
  # need this so "name" won't be dropped
         name = max(name), 
  # change grouping
  .groups = "drop") %>%
  group_by(order_ym) %>% 
  # rank n_orders and sum_quantity (1=most orders/largest quantity)
  mutate("rank_orders" = dense_rank(desc(n_orders)),
         "rank_quantity" = dense_rank(desc(sum_quantity))) %>% 
  # get top three products by n_orders
  filter(rank_orders %in% c(1,2,3)) %>% 
  # for repeat rank_orders, choose the record(s) with the largest quantity ordered
  group_by(rank_orders, .add=TRUE) %>%
  filter(rank_quantity == min(rank_quantity)) %>% 
  arrange(order_ym) %>%
  ungroup() %>%
  select(order_ym,
         name) %>%
  group_by(order_ym) %>% 
  nest() %>% 
  rename("top_products"=data)%>%
  mutate(top_products = str_c(unlist(top_products),collapse="\n"))

# orders_by_ym_user <- data_denorm %>%
#   # number of orders for each user_type/order_ym combo
#   distinct(order_id, order_ym, user_type) %>%
#   group_by(order_ym, user_type) %>% 
#   summarize(n_orders=n(), .groups = "keep")
# 
# # plot
# 
# p <- orders_by_ym_user %>% 
#   left_join(top_products_by_ym_user, by=c("user_type"="user_type", "order_ym"="order_ym")) %>%
#   ggplot(aes(fill=user_type, x=order_ym, y=n_orders, text=data)) +
#            geom_col(position="stack") 
# 
# ggplotly(p, tooltip = "text")
# 
