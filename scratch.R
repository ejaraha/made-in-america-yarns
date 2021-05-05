library(ggplot2)
library(plotly)

setwd("C:/Users/sjara/git/made-in-america-yarns/data/denormalized")
source("C:/Users/sjara/git/made-in-america-yarns/functions.R")

order <- read.csv("order.csv", stringsAsFactors=FALSE)
product <- read.csv("product.csv", stringsAsFactors = FALSE)



order <- order %>%
  mutate("order_ym" = format(as.Date(order_date), "%Y-%m"),
         "order_y" = year(as.Date(order_date)),
         "order_m" = month(as.Date(order_date))) 


order <- order %>% left_join(product, by=c("product_id"="product_id", "variation_id"="variation_id")) %>% 
rename("name"=name.x,
       "color"=color.x)%>%
  select(-c(name.y, color.y)) 

# > df %>%
#   +   group_by(customer_name) %>%
#   +   mutate(good_ranks = order(order(order_values, decreasing=TRUE)))

top_products_by_ym_user <- order %>%
  group_by(order_ym, user_type, product_id) %>%
  # number of orders for each product_id/user_type/order_ym combo
  mutate("hover.a" = n()) %>%
  ungroup() %>%
  group_by(order_ym, user_type) %>%
  # find top three products by user_type/order_ym
  mutate("hover.b" = dense_rank(desc(hover.a))) %>%
  filter(hover.b %in% c(1,2,3)) %>% 
  distinct(product_id, name, user_type, order_ym) %>% 
  select(-product_id) %>%
  arrange(name)%>%
  nest() %>%
  mutate(data = str_c(unlist(data),collapse="\n")) 

orders_by_ym_user <- order %>%
  # number of orders for each user_type/order_ym combo
  distinct(order_id, order_ym, user_type) %>%
  group_by(order_ym, user_type) %>% 
  summarize(n_orders=n(), .groups = "keep")

p <- orders_by_ym_user %>% 
  left_join(top_products_by_ym_user, by=c("user_type"="user_type", "order_ym"="order_ym")) %>%
  ggplot(aes(fill=user_type, x=order_ym, y=n_orders, text=data)) +
           geom_col(position="stack") 

ggplotly(p, tooltip = "text")

