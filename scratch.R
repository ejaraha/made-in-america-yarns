df <- data_denorm %>%
  # # filter sidebar selections
  # filter(order_date >= input$date_range[1],
  #        order_date <= input$date_range[2],
  #        customer_type %in% input$customer_type,
  #        meta.yarn_usage %in% input$customer_usage,
  #        fiber %in% input$yarn_fiber,
  #        yarn_weight %in% input$yarn_weight) %>%
  # effect %in% input$yarn_effect) %>%
  # get order-level data and all necessary fields
  distinct(order_id, product_id, order_year, order_month, order_week, name, quantity) %>%
  # get product-level info
  group_by(order_year, order_month, product_id) %>%
  # n_orders = orders placed by year/mo
  summarize(n_orders = n(),
            # sum_quantity = number purchased per period
            sum_quantity = sum(quantity),
            # need this so "name" won't be dropped
            name = max(name),
            # change grouping
            .groups = "drop") %>%
  group_by(order_year, order_month) %>%
  # rank n_orders and sum_quantity (1=most orders/largest quantity)
  mutate("rank_orders" = dense_rank(desc(n_orders)),
         "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
  # get top three products 
  filter(rank_orders %in% c(1,2,3))  %>%          
# for repeat rank_orders, choose the record(s) with the largest quantity ordered
group_by(order_year, order_month, rank_orders, .add=TRUE) %>%
  filter(rank_quantity == min(rank_quantity))