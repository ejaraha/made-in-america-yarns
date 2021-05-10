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

df <- data_denorm %>%
  mutate("order_year" = year(order_date),
         "order_month" = month(order_date),
         "order_week" = week(order_date)) %>%
  # filter according to selection
  # get order-level data and all necessary fields
  distinct(order_id, product_id, order_year, order_month, name, quantity) %>% 
  # ensure sample size of >=20
  group_by(order_month, order_week) %>% 
  summarise("orders_placed" = n()) %>% 
  filter(orders_placed >= 20) %>%
  # get product-level info
  group_by(order_year, order_month, product_id) %>% 
  # n_orders = orders placed by year/mo
  summarize(n_orders = n(),
  # sum_quantity = number of products purchased
         sum_quantity = sum(quantity),
  # need this so "name" won't be dropped
         name = max(name), 
  # change grouping
  .groups = "drop") %>%
  group_by(order_year, order_month) %>% 
  # rank n_orders and sum_quantity (1=most orders/largest quantity)
  mutate("rank_orders" = dense_rank(desc(n_orders)),
         "rank_quantity" = dense_rank(desc(sum_quantity))) %>% 
  # get top three products by n_orders
  filter(rank_orders %in% c(1,2,3)) %>% 
  # for repeat rank_orders, choose the record(s) with the largest quantity ordered
  group_by(rank_orders, .add=TRUE) %>%
  filter(rank_quantity == min(rank_quantity)) %>% 
  # count how many times each name appears in the top
  ungroup() %>%
  count(name)


df %>%
  # order by number of times in top three
  arrange(n) %>%
  mutate(name=factor(name, levels=name, ordered = TRUE)) %>%
  # plot
  ggplot(aes(x=name, y=n)) +
           geom_col() +
  coord_flip()



