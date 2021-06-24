ga <- read.csv("C:/Users/sjara/Downloads/ga_april.csv", stringsAsFactors = FALSE)
wp <- read.csv("C:/Users/sjara/Downloads/wp_april.csv", stringsAsFactors = FALSE) %>% 
  select(-contains(c("line_item", "billing")))

wp_only <- wp %>% 
  left_join(ga, by=c("order_id"="Transaction.ID")) %>% 
  filter(is.na(Date)==TRUE) %>%
  glimpse()

both <- wp %>% 
  left_join(ga, by=c("order_id"="Transaction.ID")) %>% 
  filter(is.na(Date)==FALSE) %>%
  glimpse()