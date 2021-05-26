# create role df
# make a list of lists to unpack users with multiple roles
role_list <- strsplit(data$role_main[["wp.capabilities"]],":true") %>%
  # remove anything that isn't a letter
  lapply(.,FUN = function(x){str_replace_all(x,"[^a-z]", "")})
# create a data frame with space for a user_id for each role
role_df <- data.frame("user_id" = rep(data$role_main[["id"]], sapply(role_list, length)),
                      "role" = unlist(role_list)) %>%
  mutate(across(.cols = contains("_id"), as.character)) %>% 
  # then filter to isolate wholesale buyers
  filter(role == "ignitelevelwholesalebuyer")

data$role_main %>% 
  mutate(id = as.character(id)) %>%
  inner_join(role_df, by = c("id"="user_id")) %>% 
  select(user.email) %>%
  write.csv()

                              