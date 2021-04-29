# library(dplyr)
# library(tidyr)
# library(lubridate)

# import all data
#--------------------------------------------------------------

import_data <- function(){

  # make a list of the file names in the working directory
  file_list <- list.files()
  # read all of the files into one list
  df_list <- lapply(file_list, 
                    FUN = function(f){
                      
                      df <- read.csv(f) %>% 
                        # convert character fields to lowercase
                        mutate_if(is.character, tolower)
            
                      # remove BOMs from column names 
                      # convert column names to lowercase
                      df <- rename_with(df, ~ tolower(gsub("ï..", "", .x, fixed=TRUE)))
                      
                      return(df)
                    })
  # name each item in the list using the file name
  df_names <- str_replace(file_list, ".csv", "")
  names(df_list) <- df_names
  
  return(df_list)
}

# sort column names alphabetically
#--------------------------------------------------------------------------------

sort_cols <- function(df, decrease = FALSE){
  T_F <- as.logical(decrease)
  df <- df[sort(names(df), decreasing = T_F)]
  return(df)
}


# drop unnecessary fields
#--------------------------------------------------------------------------------

drop_cols <- function(df_list){
  
  # get data
  df_list <- data
  # split df_list into two lists & order lists alphabetically in prep to loop
  ## cols: dfs of column names and boolean field of keep or not keep
  cols_df_list <- df_list[grepl("_cols", names(df_list))] %>% sort_cols()
  ## raw: raw dfs
  raw_df_list <- df_list[grepl("_raw", names(df_list))] %>% sort_cols()
  
  # filter and select cols_df_list
  cols_df_list <- lapply(cols_df_list, 
                         function(x)
                           filter(x, keep==1) %>%
                           select(variable))
  
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


# clean order_main df
#--------------------------------------------------------------

clean_order_main <- function(df){
  # initialize df
  df <- df_list$order_main

  df <- df %>%
    # redefine the existing customer_id column to be more appropriate
    rename(user_id = customer_id) %>%
    # replace "" with NA, convert to lowercase
    mutate_if(is.character, list(~na_if(tolower(.),""))) %>%
    mutate(user_id = na_if(user_id, 0)) %>%
    # only orders in us & usd
    filter(shipping_country == "us",
           order_currency == "usd") %>%
    select(-c(shipping_country, order_currency)) %>%
    # create customer_id column based on billing_email
    group_by(billing_email) %>%
    mutate("customer_id" = cur_group_id()) %>%
    ungroup()
}


# normalize data
#--------------------------------------------------------------



# normalize data
#-----------------------------------------------------------------
  
create_normalized_dfs <- function(df_list){
  df_list <- data

  ###############
  # order_
  ###############

  order <- df_list$order_main %>% 
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
  #------------------
  
  order_main_pivot <- df_list$order_main %>%
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
  #------------------
  
  order_product <- order_main_pivot %>%
    select(order_id,
           product_id,
           variation_id,
           quantity) 
  #------------------
  
  order_coupon <- df_list$order_main %>%
    mutate("coupon" = str_sub(str_trim(str_extract(coupon_items, "(?<=code:).*(?=amount:)")), start=1, end=-2)) %>%
    select(order_id,
           coupon)
  
  ###############
  # product_
  ###############
  product <- order_main_pivot %>%
    select(product_id,
           variation_id,
           name,
           color) 
  
  #------------------
  
  product_category <- df_list$product_main %>% 
    mutate("fiber" = str_extract_all(categories, "(?<=fiber > )[^,]*"), 
           "yarn_weight" = str_extract_all(categories, "(?<=yarn weight > )[^,]*"),
           "effect" = str_extract_all(categories, "(?<=effect > )[^,]*"),
           "hue" = str_extract_all(categories, "(?<=hue > )[^,]*"),
           "big_cones" = str_detect(categories, "big cones"),
           "spools" = str_detect(categories, "spools"))
  #------------------
  
  product_fiber <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>%
    unnest(fiber) %>% 
    select(product_id,
           fiber) 
  #------------------
  
  product_yarn_weight <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>%
    unnest(yarn_weight) %>% 
    select(product_id,
           yarn_weight) 
  
  #------------------
  
  product_effect <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>%
    unnest(effect) %>% 
    select(product_id,
           effect)
  
  #------------------
  
  # to be manually created by intern
  ###########
  # product_hue <- df_list$product_main %>%
  #   select(product_id,
  #          variation_id,
  #          hue)
  
  #------------------
  product_usage <- order_main_pivot %>%
    distinct(product_id, meta.yarn_usage) %>%
    select(product_id,
           meta.yarn_usage)
  
  
  ###############
  # customer_
  ###############
  
  customer <- df_list$order_main %>%
    distinct(customer_id,
             billing_email,
             user_id) 
  #------------------
  
  customer_usage <- df_list$order_main %>%
    distinct(customer_id,
             meta.yarn_usage)
  #------------------
  
  # wait until smart manager plugin is fixed
  #################
  # customer_role <- df_list$order_main %>%
  #   left_join(df_list$role_main) %>%
  #   select(customer_id,
  #          role)


}












