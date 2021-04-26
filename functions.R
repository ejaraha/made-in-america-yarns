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
  df <- data$order_main

  df <- df %>%
    # redefine the existing customer_id column to be more appropriate
    rename(registered_customer = customer_id) %>%
    # replace "" with NA, convert to lowercase
    mutate_if(is.character, list(~na_if(tolower(.),""))) %>%
    mutate(registered_customer = na_if(registered_customer, 0)) %>%
    # only orders in us & usd
    filter(shipping_country == "us",
           order_currency == "usd") %>%
    select(-c(shipping_country, order_currency)) %>%
    # create customer_id column based on billing_email
    group_by(billing_email) %>%
    mutate("customer_id" = cur_group_id()) %>%
    ungroup()
}



# clean product_main df
#--------------------------------------------------------------

clean_product_main <- function(df){
  
  df <- data$product_main

  df <- df %>% 
  # isolate name field to group products by name and assign a unique id to each group
  separate(name, into=c("name", "attribute.all"), sep="-", extra="merge", fill="right") %>%
  select(-attribute.all) %>%
  mutate(name = str_trim(name)) %>%
  group_by(name) %>%
  mutate("product_group"=cur_group_id()) %>%
  ungroup() 
  
  return(df)
}











