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



# normalize data
#-----------------------------------------------------------------
  
create_normalized_dfs <- function(df_list){
  # use the updated data to create normalized data frames
  # return a list of the normalized data frames
  
  # get list of updated data
  df_list <- data
  
  # initialize list for normalized data frames
  data_norm <- list()
  
  ###############
  #
  #
  #
  # create normalized data frames:
  #
  #
  #
  ###############
  # order_
  ###############
  #
  
  df_list$order_main <- df_list$order_main %>%
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
  

  data_norm$order <- df_list$order_main %>% 
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
  
  

  # handle three scenarios of product_id == "0" and/or variation_id == NA
  
  order_main_pivot <- order_main_pivot %>% 
    # [1] when there's a variation_id but no product_id:
    ## WooCommerce' mistake? the id listed as variation_id should actually be in the product_id column 
    ## comparing against wcSmartManager plugin shows this
    ## for these instances, swap the contents of product_id and variation_id
    mutate(product_id = case_when(product_id == "0" & is.na(variation_id) == FALSE ~ variation_id,
                                  TRUE ~ as.character(product_id)),
           # variation_id = "9999" indicates a missing variation_id
           # this is likely due to data loss
           variation_id = case_when(product_id == variation_id ~"9999",
                                    TRUE ~ as.character(variation_id))
           ) %>%
    # [2] when product_id == "0" & variation_id is.na 
    ## these are products that have since been deleted
    ## these products will also be deleted from the database from this date on
    filter(product_id != "0" & is.na(variation_id)!=TRUE) %>%   
    # [3] for the remaining records, is.na(variation_id)=TRUE indicates a simple product
    ## all NAs in variation_id will be replaced with "0000" to explicitly indicate simple products
    mutate(variation_id = case_when(is.na(variation_id)==TRUE ~ "0000",
                                    TRUE ~ as.character(variation_id)))
    
    
  #------------------
  
  data_norm$order_product <- order_main_pivot %>%
    select(order_id,
           product_id,
           variation_id,
           quantity) 
  
  #------------------
  
  data_norm$order_coupon <- df_list$order_main %>%
    mutate("coupon" = str_sub(str_trim(str_extract(coupon_items, "(?<=code:).*(?=amount:)")), start=1, end=-2)) %>%
    filter(is.na(coupon)==FALSE) %>%
    select(order_id,
           coupon)
  
  ###############
  # product_
  ###############
  data_norm$product <- order_main_pivot %>%
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
  
  data_norm$product_fiber <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>%
    unnest(fiber) %>% 
    select(product_id,
           fiber) 
  
  #------------------
  
  data_norm$product_yarn_weight <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>% 
    unnest(yarn_weight) %>% 
    select(product_id,
           yarn_weight) 
  
  #------------------
  
  data_norm$product_effect <- product_category %>%
    filter(type == "variable") %>%
    rename("product_id"=id) %>%
    unnest(effect) %>% 
    select(product_id,
           effect)
  
  #------------------
  
  # to be manually created
  ###########
  # product_hue <- df_list$product_main %>%
  #   select(product_id,
  #          variation_id,
  #          hue)
  
  #------------------
  
  # unpack meta.yarn_usage with multiple usages
  usage_list <- strsplit(order_main_pivot$meta.yarn_usage, split = ",")
  # create a new data frame with room to insert the usages previously packed in comma-separated character strings
  data_norm$product_usage <- data.frame(product_id = rep(order_main_pivot$product_id, sapply(usage_list, length)), 
                                        meta.yarn_usage = unlist(usage_list)) %>%
    filter(is.na(meta.yarn_usage)==FALSE)
  
  
  
  ###############
  # customer_
  ###############
  
  data_norm$customer <- df_list$order_main %>%
    distinct(customer_id,
             billing_email,
             user_id) 
  #------------------
  
  data_norm$customer_usage <- data.frame(customer_id = rep(order_main_pivot$customer_id, sapply(usage_list, length)), 
                                         meta.yarn_usage = unlist(usage_list)) %>%
    filter(is.na(meta.yarn_usage)==FALSE)
  
  #------------------
  
  # wait until smart manager plugin is fixed
  #################
  # customer_role <- df_list$order_main %>%
  #   left_join(df_list$role_main) %>%
  #   select(customer_id,
  #          role)


  return(data_norm)
}

define_primary_keys <- function(){

primary_keys <- list(
  "order_pk" = data_norm$order %>% 
    select(order_id),
  "order_product_pk" = data_norm$order_product %>%
    select(order_id,
           product_id,
           variation_id),
  "order_coupon_pk" = data_norm$order_coupon %>%
    select(everything()),
  "product_pk" = data_norm$product %>%
    select(product_id,
           variation_id),
  "product_fiber_pk" = data_norm$product_fiber %>%
    select(everything()),
  "product_yarn_weight_pk" = data_norm$product_yarn_weight %>%
    select(everything()),
  "product_effect_pk" = data_norm$product_effect %>%
    select(everything()),
  "product_usage_pk" = data_norm$product_usage %>%
    select(everything()),
  "customer_pk" = data_norm$customer %>%
    select(customer_id),
  "customer_usage_pk" = data_norm$customer_usage %>%
    select(everything())
) 
return(primary_keys)
}


check_primary_keys <- function(pk_list){
  
  # get list of primary key data frames
  
  # create a data frame to record the result of each primary key data frame against the criteria:
  ## is_unique? is_na? is_empty_string?
  criteria_df <- as.data.frame(
    cbind(is_unique = lapply(pk_list, function(pk){if_else(count(pk)==nrow(pk),
                                                     TRUE,
                                                     FALSE)}),
          is_na = lapply(pk_list, function(pk){any(is.na(pk))}),
          is_empty_string = lapply(pk_list, function(pk){any(pk=="", na.rm = TRUE)})
    )
  )
  
  # create a data frame of only invalid primary keys
  invalid <- criteria_df %>% filter(is_unique == FALSE |is_na == TRUE |is_empty_string == TRUE)
  
  # if there are any invalid primary keys, return those
  if(nrow(invalid) != 0){
    print("INVALID PRIMARY KEYS:")
    result <- invalid
    # if not, all good!
  }else{result <- "All good! Carry on..."}
  
  
  return(result)
  
}


check_empty <- function(df){
  #checks every column in df for NAs and ""
  df <- data.frame("any_nas" = apply(df, 2, function(df) any(is.na(df))),
                   "any_empty_character_vectors" = apply(df, 2, function(df) any(df == "", na.rm = TRUE))) 
  return(df)
}





