
product_category <- data$product_main %>% 
  mutate("fiber" = str_extract_all(categories, "(?<=fiber > )[^,]*"), 
         "yarn_weight" = str_extract_all(categories, "(?<=yarn weight > )[^,]*"),
         "effect" = str_extract_all(categories, "(?<=effect > )[^,]*"),
         "hue" = str_extract_all(categories, "(?<=hue > )[^,]*"),
         "big_cones" = case_when(str_detect(categories, "big cones")==TRUE ~"big cones",
                                 TRUE ~ NA_character_),
         "spools" = case_when(str_detect(categories, "spools")==TRUE ~"spools",
                              TRUE ~ NA_character_),
         id = as.character(id))

test <- product_category %>% filter(type != "variation")

test_extract <- test %>% 
  mutate("fiber" = str_extract_all(categories, "(?<=fiber > )[^,]*"), 
         "yarn_weight" = str_extract_all(categories, "(?<=yarn weight > )[^,]*"),
         "effect" = str_extract_all(categories, "(?<=effect > )[^,]*"),
         "hue" = str_extract_all(categories, "(?<=hue > )[^,]*"),
         "big_cones" = case_when(str_detect(categories, "big cones")==TRUE ~"big cones",
                                 TRUE ~ NA_character_),
         "spools" = case_when(str_detect(categories, "spools")==TRUE ~"spools",
                              TRUE ~ NA_character_),
         id = as.character(id))

test_extract %>% filter(is.na(yarn_weight)==TRUE) %>% glimpse()

test <- data$product_main %>% filter(id %in% c("22215", "22210", "22211", "22211", "22213", "12386", "12384")) %>% glimpse()

