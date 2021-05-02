# library(dplyr)
# library(tidyr)
# library(lubridate)

# import all data
#-------------------------------------------------------------
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
  
  # output messages
  cat("All necessary files imported:\n----------------------------\n")
  cat(file_list, sep="\n")
  
  return(df_list)
}

# sort column names alphabetically
#-------------------------------------------------------------
sort_cols <- function(df, decrease = FALSE){
  T_F <- as.logical(decrease)
  df <- df[sort(names(df), decreasing = T_F)]
  return(df)
}

# drop unnecessary fields
#-------------------------------------------------------------
drop_cols <- function(df_list){
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
    raw_drop_df_list[[i]] <- df}
  names(raw_drop_df_list) <- lapply(names(raw_df_list), function(x)paste(x, "_drop", sep=""))
  #output messages
  df_names <- names(raw_drop_df_list)
  cat(sprintf("\n%i new data frames have been created\nwith only the necessary fields from the _raw data frames:
              \n--------------------------------------
              \n", length(raw_drop_df_list)))
  cat(df_names, sep="\n")
  return(raw_drop_df_list)
}

# update data
#-------------------------------------------------------------
update <- function(data){
  # import all data (_raw.csv, _cols.csv, _main.csv)
  #--------------------------------------------------------------
  data <- import_data() %>% sort_cols()
  
  # drop unnecessary fields from raw data (_raw_drop.csv)
  #--------------------------------------------------------------
  ## add the _raw_drop files to the data list
  data <- append(data, drop_cols(data))
  
  # output message: number of rows BEFORE update
  nrow_om_before <- as.integer(nrow(data$order_main))
  nrow_pm_before <- as.integer(nrow(data$product_main))
  nrow_rm_before <- as.integer(nrow(data$role_main))
  cat(sprintf("\n\nThe _main data frames will now be updated with _raw_drop data frames
              \n-----------------------------------------------------------------------
              \n...nrows in _main data frames BEFORE the update:
              \ndata$order_main %i\ndata$product_main %i\ndata$role_main %i",
              nrow_om_before, 
              nrow_pm_before,
              nrow_rm_before))
  
  # update main data with most recent data (_main <- update(_main + _raw_drop))
  #--------------------------------------------------------------
  ## update matching rows from order_main to match order
  ## & insert non-matching ids from order_raw_drop into order_main
  data$order_main <- rows_upsert(data$order_main, data$order_raw_drop, by="order_id")
  
  ## update matching rows from product_main to match product
  ## & insert non-matching rows from product_raw_drop to product_main
  data$product_main <- rows_upsert(data$product_main,data$product_raw_drop, by="id")
  
  ## update matching rows from role_main to match role
  ## & insert non-matching rows from role_raw_drop to role_main
  data$role_main <- rows_upsert(data$role_main, data$role_raw_drop, by="id")
  
  # output message: number of rows AFTER update
  cat(sprintf("\n\n...nrows ADDED TO _main data frames during the update:
              \ndata$order_main %i\ndata$product_main %i\ndata$role_main %i\n",
              as.integer(nrow(data$order_main) - nrow_om_before),
              as.integer(nrow(data$product_main)) - nrow_pm_before,
              as.integer(nrow(data$role_main)) - nrow_rm_before))
  
  return(data)
}

# normalize data
#-------------------------------------------------------------
normalize <- function(df_list){
  # use the updated data to create normalized data frames
  # return a list of the normalized data frames
  
  # initialize list for normalized data frames
  data_norm <- list()
  #
  # create normalized data frames:
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
  
  cat(sprintf("\n\n%i normalized data frames created:\n", length(data_norm)))
  cat(names(data_norm), sep="\n")
  return(data_norm)
}

primary_key_list <- function(){
  # select the primary key fields from [data_norm]
  ## in the global environment, [data_norm] is a variable defined by 'normalize(update_db())'
  df_list <- list(
    "pk_order" = data_norm$order %>% 
      select(order_id),
    "pk_order_product" = data_norm$order_product %>%
      select(order_id,
             product_id,
             variation_id),
    "pk_order_coupon" = data_norm$order_coupon %>%
      select(everything()),
    "pk_product" = data_norm$product %>%
      select(product_id,
             variation_id),
    "pk_product_fiber" = data_norm$product_fiber %>%
      select(everything()),
    "pk_product_yarn_weight" = data_norm$product_yarn_weight %>%
      select(everything()),
    "pk_product_effect" = data_norm$product_effect %>%
      select(everything()),
    "pk_product_usage" = data_norm$product_usage %>%
      select(everything()),
    "pk_customer" = data_norm$customer %>%
      select(customer_id),
    "pk_customer_usage" = data_norm$customer_usage %>%
      select(everything()))
  
  return(df_list)
}

check_primary_keys <- function(df_list){
  
  # get list of primary key data frames
  pk_list <- df_list
  # [1] check each pk in pk_list for uniqueness, NAs, and empty strings. 
  ## record the results in criteria_df
  pk_check <- as.data.frame(
    cbind(is_unique = lapply(pk_list, function(pk){if_else(count(pk)==nrow(pk),
                                                           # unique if the # rows matches the # unique rows
                                                           TRUE,
                                                           FALSE)}),
          is_na = lapply(pk_list, function(pk){any(is.na(pk))}),
          is_empty_string = lapply(pk_list, function(pk){any(pk=="", na.rm = TRUE)})))
  # [2] create a data frame of only invalid primary keys 
  invalid <- pk_check %>% 
    filter(is_unique == FALSE |is_na == TRUE |is_empty_string == TRUE)
  # [3] if there are any invalid primary keys, return those
  if(nrow(invalid) != 0){
    cat("INVALID PRIMARY KEYS: \n The following dataframes have invalid primary keys:")
    result <- invalid
    # if not, all good!
  }else{result <- "All primary keys are valid (unique, no nulls)."}
  
  return(result)
}

check_empty <- function(df){
  #checks every column in df for NAs and ""
  df <- data.frame("any_nas" = apply(df, 2, function(df) any(is.na(df))),
                   "any_empty_character_vectors" = apply(df, 2, function(df) any(df == "", na.rm = TRUE))) 
  return(df)
}



