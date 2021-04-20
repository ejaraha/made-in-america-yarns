library(dplyr)
library(tidyr)
library(lubridate)

# import all data
#--------------------------------------------------------------

import_data <- function(){

  # make a list of the file names in the working directory
  file_list <- list.files()
  # read all of the files into one list
  df_list <- lapply(file_list, 
                    FUN = function(f){
                      read.csv(f)
                    })
  # name each item in the list using the file name
  df_names <- str_replace(file_list, ".csv", "")
  names(df_list) <- df_names
  
  return(df_list)
}


# CLEAN order_main df
#--------------------------------------------------------------

clean_order_main <- function(df){
  # initialize df
  df <- data$order_main
  # create customer_id column based on billing_email
  id_df <- df %>%
    distinct(billing_email) %>%
    mutate(customer_id=1:nrow(.)) 
  df <- df %>%
    # redefine the existing customer_id column to be more appropriate
    rename(registered_customer = customer_id) %>%
    # join the customer_id column defined above
    left_join(.,id_df, by="billing_email") %>%
    # replace "" with NA, convert to lowercase
    mutate_if(is.character, list(~na_if(tolower(.),""))) %>%
    # eliminate all orders outside of the US
    filter(shipping_country == "us") %>%
    select(-shipping_country)
}
















