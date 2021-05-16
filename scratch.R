
# [1] join order level data
data_denorm <- data_norm$order_product %>%
  left_join(data_norm$order, by="order_id") %>%
  left_join(data_norm$order_coupon, by="order_id") %>%
  left_join(data_norm$order_usage, by="order_id") %>%
  # [2] rejoin product data
  left_join(data_norm$product, by=c("product_id"="product_id", "variation_id"="variation_id")) %>%
  left_join(data_norm$product_hue, by=c("product_id"="product_id", "variation_id"="variation_id")) %>%
  left_join(data_norm$product_fiber, by="product_id") %>%
  left_join(data_norm$product_yarn_weight, by="product_id") %>%
  left_join(data_norm$product_effect, by="product_id") %>%
  rename("name"=name.x,
         "color"=color.x)%>%
  select(-c(name.y, color.y)) %>% filter(is.na(yarn_weight)==TRUE)