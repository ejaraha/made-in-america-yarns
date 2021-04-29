library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

check_empty <- function(df){
  #checks every column in df for NAs and ""
  df <- data.frame("any_nas" = apply(df, 2, function(df) any(is.na(df))),
                   "any_empty_character_vectors" = apply(df, 2, function(df) any(df == "", na.rm = TRUE))) 
  return(df)
}

# IMPORT DATA
#--------------------------------------------------------------

setwd("C:/Users/sjara/git/made-in-america-yarns/data")

file_list <- list.files()

df_list <- lapply(file_list, 
               FUN = function(f){
                 read.csv(f)
               })

df_names <- str_replace(file_list, ".csv", "")
names(df_list) <- df_names




# # set working directory
# setwd("C:/Users/sjara/git/made-in-america-yarns/data/new")
# 
# # load keys to select necessary fields
# order_cols <- read.csv("order_cols.csv") %>%
#   filter(keep==1)
# product_cols <- read.csv("product_cols.csv") %>%
#   filter(keep==1)
# 
# # load raw data then select necessary fields
# order <- read.csv("order.csv") %>%
#   select(order_cols$variable)
# product <- read.csv("product.csv") %>%
#   select(product_cols$variable)

# CLEAN order_main df
#--------------------------------------------------------------

id_df <- data$order_main %>%
  distinct(billing_email) %>%
  mutate(customer_id=1:nrow(.)) 

data$order_main <- data$order_main %>%
  select(-customer_id) %>%
  left_join(.,id_df, by="billing_email") %>%
  mutate_if(is.character, list(~na_if(tolower(.),""))) %>%
  filter(shipping_country == "us") %>%
  select(-shipping_country)

# CLEAN product_main df
#--------------------------------------------------------------


# clean product_main df
#--------------------------------------------------------------
# 
# clean_product_main <- function(df){
#   
#   df <- data$product_main
# 
#   df <- df %>% 
#   # isolate name field to group products by name and assign a unique id to each group
#   separate(name, into=c("name", "attribute.all"), sep="-", extra="merge", fill="right") %>%
#   select(-attribute.all) %>%
#   mutate(name = str_trim(name)) %>%
#   group_by(name) %>%
#   mutate("product_group"=cur_group_id()) %>%
#   ungroup() 
#   
#   return(df)
# }

#names_with_hyphens <-c(9824,9825,9826,9827,9828,9829,9830,9850,10078,14822,14823,14824,14825,14863,15022,15025,15526,15529,15713,18075,18076,18189,18190,18963,18964,18965,18966,18967,18968,18969,22036,22037,22159,22161,22162,22322)

df <- data$product_main %>% 
  # # handle names with "-" before the "-" that marks the start of the attributes 
  # # so that we can use the "-" to isolate the name
  # mutate(Name = case_when(ID %in% names_with_hyphens ~ str_replace(Name, "-", ""),
  #                         TRUE ~ Name)) %>%
  # # the ID field will be re-purposed as the variation_id field
  # rename("variation_id"=ID) %>%
  
  # isolate name field to group products by name and assign a unique id to each group (used for SKU)
  separate(Name, into=c("Name", "Attribute.all"), sep="-", extra="merge", fill="right") %>%
  mutate(Name = str_trim(Name)) %>%
  group_by(Name) %>%
  # order by name, then ID
  arrange(as.integer(ID), .by_group = TRUE) %>%
  mutate("product_group"=cur_group_id()) %>%
  ungroup() 


# %>%
#   # create the SKU = (P + product_id + V + variation_id)
#   unite("sku", c("product_id", "variation_id"), sep="V", remove = FALSE) %>%
#   mutate(sku = case_when(Type != "variable" ~ paste("P", sku, sep=""),
#                          TRUE ~ paste("P", product_id, sep="")))


write.csv(df, "C:/Users/sjara/Downloads/sku.csv", row.names = FALSE)

# NORMALIZE 
#--------------------------------------------------------------

# MAKE order df
#--------------------------------------------------------------

order <- data$order_main %>%
  mutate(order_id = as.character(order_id),
         order_date = ymd_hms(order$order_date),
         order_hour = hour(order_date),
         order_date = as_date(ymd_hms(order$order_date)))

# 
# write.csv(order, file="C:/Users/sjara/git/made-in-america-yarns/data/live/order.csv", row.names=FALSE)
# glimpse(order)
# check_empty(order)

# drop unnecessary fields from raw data
#--------------------------------------------------------------

drop_cols <- function(df_list){

  # get data
  df_list <- data
  # split df_list into two lists
  ## cols: dfs of column names and boolean field of keep or not keep
  cols_df_list <- df_list[grepl("_cols", names(data))]
  ## raw: raw dfs
  raw_df_list <- df_list[grepl("_raw", names(data))]
  
  # filter and select cols_df_list
  cols_df_list <- lapply(cols_df_list, 
                         function(x)
                           filter(x, keep==1) %>%
                           select(variable))
  
  # order lists alphabetically in prep to loop
  cols_df_list <- cols_df_list[order(names(cols_df_list))]
  raw_df_list <- raw_df_list[order(names(raw_df_list))]
  
  # initialize list for loop
  raw_drop_df_list <- list()
  
  for(i in 1:length(raw_df_list)){
    
    # define variables
    ## column names to keep
    cols <- cols_df_list[[i]]$variable
    ## corresponding df
    df <- raw_df_list[[i]]
    
    # redefine df with only columns %in% cols
    df <- df %>%
      select(all_of(cols))
    
    # add df to the list of processed dfs
    raw_drop_df_list[[i]] <- df
  }
  
  names(raw_drop_df_list) <- lapply(names(raw_df_list), function(x)paste(x, "_drop", sep=""))

  return(raw_drop_df_list)
}

## order
data$order_cols <- data$order_cols %>%
  filter(keep == 1)
data$order_raw_drop <- data$order_raw %>%
  select(data$order_cols$variable)
## product
data$order_cols <- data$product_cols %>%
  filter(keep == 1)
product_raw_drop <- data$product_raw %>%
  select(data$order_cols$variable)




# normalize data
#-----------------------------------------------------------------

# order_
###############
order <- data$order_main %>% 
  # extract date from order_date
  mutate(order_date = as_date(ymd_hms(order_date)),
         # convert ids to character
         order_id = as.character(order_id),
         customer_id = as.character(customer_id)) %>%
  select(order_id,
         order_date,
         order_total,
         customer_id,
         billing_company)

order_main_pivot <- data$order_main %>%
  # gather all "line_item_" columns
  pivot_longer(cols = starts_with("line_item"), 
               names_to = "item", 
               values_to = "item_description",
               values_drop_na = TRUE) %>% 
  # extract product info from item_description
  mutate("product_id" = str_extract(item_description, "(?<=product_id:)[:digit:]*"),
         "variation_id" = str_extract(item_description, "(?<=variation_id:)[:digit:]*"),
         "quantity" = str_extract(item_description, "(?<=quantity:)[:digit:]*(?=|total)"),
         "name" = str_sub(str_trim(str_extract(item_description, "(?<=name:).*(?=product_id:)")),start=1, end=-2),
         "color" = str_extract(item_description, "(?<=color:|colors:)[:alpha:]*[:space:]*[:alpha:]*[:space:]*[:alpha:]*")) %>%
  separate(name, into = c("name", "detail"), sep = "-", extra="merge", fill="right") %>%
  mutate_at(c("name", "detail"), str_trim) 

# handle error
order_main_pivot <- order_main_pivot %>% 
  # this is to handle an error in some of the entries of order_raw$line_item_
  # when product_id="0" & variation_id!=NA, then swap product_id and variation_id
  mutate(product_id = case_when(product_id == "0" & is.na(variation_id) == FALSE ~variation_id,
                       TRUE ~ as.character(product_id)),
         variation_id = case_when(product_id == variation_id ~"0",
                         TRUE ~ as.character(variation_id)))


order_product <- order_main_pivot %>%
  select(order_id,
         product_id,
         variation_id,
         quantity) 

order_coupon <- data$order_main %>%
  mutate("coupon" = str_sub(str_trim(str_extract(coupon_items, "(?<=code:).*(?=amount:)")), start=1, end=-2)) %>%
  select(order_id,
         coupon)


# product_
####################
product <- order_main_pivot %>%
  select(product_id,
         variation_id,
         name,
         color) 


product_category <- data$product_main %>% 
  mutate("fiber" = str_extract_all(categories, "(?<=fiber > )[^,]*"), 
         "yarn_weight" = str_extract_all(categories, "(?<=yarn weight > )[^,]*"),
         "effect" = str_extract_all(categories, "(?<=effect > )[^,]*"),
         "hue" = str_extract_all(categories, "(?<=hue > )[^,]*"),
         "big_cones" = str_detect(categories, "big cones"),
         "spools" = str_detect(categories, "spools"))
 

glimpse(product_category)

product_fiber <- product_category %>%
  filter(type == "variable") %>%
  rename("product_id"=id) %>%
  unnest(fiber) %>% 
  select(product_id,
         fiber) 

product_yarn_weight <- product_category %>%
  filter(type == "variable") %>%
  rename("product_id"=id) %>%
  unnest(yarn_weight) %>% 
  select(product_id,
         yarn_weight) 

product_effect <- product_category %>%
  filter(type == "variable") %>%
  rename("product_id"=id) %>%
  unnest(effect) %>% 
  select(product_id,
         effect)

# to be manually created by intern
###########
# product_hue <- data$product_main %>%
#   select(product_id,
#          variation_id,
#          hue)

product_usage <- order_main_pivot %>%
  distinct(product_id, meta.yarn_usage) %>%
  select(product_id,
         meta.yarn_usage)


# customer_
#################
customer <- data$order_main %>%
  distinct(customer_id,
         billing_email,
         user_id) 

customer_usage <- data$order_main %>%
  distinct(customer_id,
         meta.yarn_usage)

# wait until smart manager plugin is fixed
#################
# customer_role <- data$order_main %>%
#   left_join(data$role_main) %>%
#   select(customer_id,
#          role)







