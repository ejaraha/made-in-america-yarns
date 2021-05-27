data$product_hue %>% 
  mutate(across(.cols= everything(), as.character)) %>%
  filter(if_any(.cols= contains("hue"), ~.%in% c("blue", "yellow"))) %>%
  glimpse()

