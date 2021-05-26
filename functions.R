library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(tibble)

# import all data
#-------------------------------------------------------------
import_data <- function(match_file_name){
  # make a list of the file names in the working directory
  file_list <- list.files(pattern = match_file_name)
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
  
  # standardize item columns
  cols <- c("line_item_1"= NA_character_, "line_item_2"= NA_character_, "line_item_3"= NA_character_, "line_item_4"= NA_character_, "line_item_5"= NA_character_, "line_item_6"= NA_character_, "line_item_7"= NA_character_, "line_item_8"= NA_character_, "line_item_9"= NA_character_, "line_item_10"= NA_character_, "line_item_11"= NA_character_, "line_item_12"= NA_character_, "line_item_13"= NA_character_, "line_item_14"= NA_character_, "line_item_15"= NA_character_, "line_item_16"= NA_character_, "line_item_17"= NA_character_, "line_item_18"= NA_character_, "line_item_19"= NA_character_, "line_item_20"= NA_character_, "line_item_21"= NA_character_, "line_item_22"= NA_character_, "line_item_23"= NA_character_, "line_item_24"= NA_character_, "line_item_25"= NA_character_, "line_item_26"= NA_character_, "line_item_27"= NA_character_, "line_item_28"= NA_character_, "line_item_29"= NA_character_, "line_item_30"= NA_character_, "line_item_31"= NA_character_, "line_item_32"= NA_character_, "line_item_33"= NA_character_, "line_item_34"= NA_character_, "line_item_35"= NA_character_, "line_item_36"= NA_character_, "line_item_37"= NA_character_, "line_item_38"= NA_character_, "line_item_39"= NA_character_, "line_item_40"= NA_character_, "line_item_41"= NA_character_, "line_item_42"= NA_character_, "line_item_43"= NA_character_, "line_item_44"= NA_character_, "line_item_45"= NA_character_, "line_item_46"= NA_character_, "line_item_47"= NA_character_, "line_item_48"= NA_character_, "line_item_49"= NA_character_, "line_item_50"= NA_character_, "line_item_51"= NA_character_, "line_item_52"= NA_character_, "line_item_53"= NA_character_, "line_item_54"= NA_character_, "line_item_55"= NA_character_, "line_item_56"= NA_character_, "line_item_57"= NA_character_, "line_item_58"= NA_character_, "line_item_59"= NA_character_, "line_item_60"= NA_character_, "line_item_61"= NA_character_, "line_item_62"= NA_character_, "line_item_63"= NA_character_, "line_item_64"= NA_character_, "line_item_65"= NA_character_, "line_item_66"= NA_character_, "line_item_67"= NA_character_, "line_item_68"= NA_character_, "line_item_69"= NA_character_, "line_item_70"= NA_character_, "line_item_71"= NA_character_, "line_item_72"= NA_character_, "line_item_73"= NA_character_, "line_item_74"= NA_character_, "line_item_75"= NA_character_, "line_item_76"= NA_character_, "line_item_77"= NA_character_, "line_item_78"= NA_character_, "line_item_79"= NA_character_, "line_item_80"= NA_character_, "line_item_81"= NA_character_, "line_item_82"= NA_character_, "line_item_83"= NA_character_, "line_item_84"= NA_character_, "line_item_85"= NA_character_, "line_item_86"= NA_character_, "line_item_87"= NA_character_, "line_item_88"= NA_character_, "line_item_89"= NA_character_, "line_item_90"= NA_character_, "line_item_91"= NA_character_, "line_item_92"= NA_character_, "line_item_93"= NA_character_, "line_item_94"= NA_character_, "line_item_95"= NA_character_, "line_item_96"= NA_character_, "line_item_97"= NA_character_, "line_item_98"= NA_character_, "line_item_99"= NA_character_, "line_item_100" = NA_character_, "line_item_101" = NA_character_, "line_item_102" = NA_character_, "line_item_103" = NA_character_, "line_item_104" = NA_character_, "line_item_105" = NA_character_, "line_item_106" = NA_character_, "line_item_107" = NA_character_, "line_item_108" = NA_character_, "line_item_109" = NA_character_, "line_item_110" = NA_character_, "line_item_111" = NA_character_, "line_item_112" = NA_character_, "line_item_113" = NA_character_, "line_item_114" = NA_character_, "line_item_115" = NA_character_, "line_item_116" = NA_character_, "line_item_117" = NA_character_, "line_item_118" = NA_character_, "line_item_119" = NA_character_, "line_item_120" = NA_character_, "line_item_121" = NA_character_, "line_item_122" = NA_character_, "line_item_123" = NA_character_, "line_item_124" = NA_character_, "line_item_125" = NA_character_, "line_item_126" = NA_character_, "line_item_127" = NA_character_, "line_item_128" = NA_character_, "line_item_129" = NA_character_, "line_item_130" = NA_character_, "line_item_131" = NA_character_, "line_item_132" = NA_character_, "line_item_133" = NA_character_, "line_item_134" = NA_character_, "line_item_135" = NA_character_, "line_item_136" = NA_character_, "line_item_137" = NA_character_, "line_item_138" = NA_character_, "line_item_139" = NA_character_, "line_item_140" = NA_character_, "line_item_141" = NA_character_, "line_item_142" = NA_character_, "line_item_143" = NA_character_, "line_item_144" = NA_character_, "line_item_145" = NA_character_, "line_item_146" = NA_character_, "line_item_147" = NA_character_, "line_item_148" = NA_character_, "line_item_149" = NA_character_, "line_item_150" = NA_character_, "line_item_151" = NA_character_, "line_item_152" = NA_character_, "line_item_153" = NA_character_, "line_item_154" = NA_character_, "line_item_155" = NA_character_, "line_item_156" = NA_character_, "line_item_157" = NA_character_, "line_item_158" = NA_character_, "line_item_159" = NA_character_, "line_item_160" = NA_character_, "line_item_161" = NA_character_, "line_item_162" = NA_character_, "line_item_163" = NA_character_, "line_item_164" = NA_character_, "line_item_165" = NA_character_, "line_item_166" = NA_character_, "line_item_167" = NA_character_, "line_item_168" = NA_character_, "line_item_169" = NA_character_, "line_item_170" = NA_character_, "line_item_171" = NA_character_, "line_item_172" = NA_character_, "line_item_173" = NA_character_, "line_item_174" = NA_character_, "line_item_175" = NA_character_, "line_item_176" = NA_character_, "line_item_177" = NA_character_, "line_item_178" = NA_character_, "line_item_179" = NA_character_, "line_item_180" = NA_character_, "line_item_181" = NA_character_, "line_item_182" = NA_character_, "line_item_183" = NA_character_, "line_item_184" = NA_character_, "line_item_185" = NA_character_, "line_item_186" = NA_character_, "line_item_187" = NA_character_, "line_item_188" = NA_character_, "line_item_189" = NA_character_, "line_item_190" = NA_character_, "line_item_191" = NA_character_, "line_item_192" = NA_character_, "line_item_193" = NA_character_, "line_item_194" = NA_character_, "line_item_195" = NA_character_, "line_item_196" = NA_character_, "line_item_197" = NA_character_, "line_item_198" = NA_character_, "line_item_199" = NA_character_, "line_item_200" = NA_character_)
  df_list$order_raw <- df_list$order_raw %>% add_column(!!!cols[!names(cols) %in% names(.)])

  
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
  data <- import_data(".csv") %>% sort_cols()
  
  # drop unnecessary fields from raw data (_raw_drop.csv)
  #--------------------------------------------------------------
  ## add the _raw_drop files to the data list
  data <- append(data, drop_cols(data))
  
  # output message: number of orders/products/users BEFORE update
  nrow_om_before <- as.integer(n_distinct(data$order_main["order_id"]))
  nrow_pm_before <- data$product_main %>% filter(type != "variation") %>% n_distinct("id") %>% as.integer()
  nrow_rm_before <- as.integer(n_distinct(data$role_main["id"]))
  cat(sprintf("\n\nThe _main data frames will now be updated with _raw_drop data frames
              \n-----------------------------------------------------------------------
              \n..._main counts BEFORE the update:
              \norders: %i\nproducts: %i\nusers: %i",
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
  cat(sprintf("\n\n..._main counts AFTER the update:
              \nnew orders: %i\nnew products: %i\nnew users: %i\n",
              as.integer(n_distinct(data$order_main["order_id"])) - nrow_om_before,
              data$product_main %>% filter(type != "variation") %>% n_distinct("id") %>% as.integer() - nrow_pm_before,
              as.integer(n_distinct(data$role_main["id"])) - nrow_rm_before))
  
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
    mutate(across(.cols = contains("_id"), as.character)) %>% 
    ungroup()
  
  # create role df
  # make a list of lists to unpack users with multiple roles
  role_list <- strsplit(df_list$role_main[["wp.capabilities"]],":true") %>%
    # remove anything that isn't a letter
    lapply(.,FUN = function(x){str_replace_all(x,"[^a-z]", "")})
  # create a data frame with space for a user_id for each role
  data_norm$wholesale_df <- data.frame("user_id" = rep(df_list$role_main[["id"]], sapply(role_list, length)),
                        "role" = unlist(role_list)) %>%
    mutate(across(.cols = contains("_id"), as.character)) %>% 
    # then filter to isolate wholesale buyers
    filter(role == "ignitelevelwholesalebuyer")
  
  data_norm$order <- df_list$order_main %>% 
    # extract date from order_date
    mutate(order_date = as_date(ymd_hms(order_date)),
           # convert ids to character
           order_id = as.character(order_id)) %>%
    # join role df to make customer_type column
    left_join(data_norm$wholesale_df, by="user_id") %>% 
    mutate("customer_type" = case_when(is.na(user_id)==TRUE ~ "guest",
                                   role=="ignitelevelwholesalebuyer" ~ "wholesale buyer",
                                   is.na(user_id)==FALSE ~ "registered customer")) %>%
    mutate(across(.cols = contains("_id"), as.character)) %>% 
    select(order_id,
           order_date,
           customer_type)
  
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
           "quantity" = as.integer(str_extract(item_description, "(?<=quantity:)[:digit:]*(?=|total)")),
           "name" = str_sub(str_trim(str_extract(item_description, "(?<=name:).*(?=product_id:)")),start=1, end=-2),
           "color" = str_extract(item_description, "(?<=color:|colors:)[:alpha:]*[:space:]*[:alpha:]*[:space:]*[:alpha:]*")) %>%
    separate(name, into = c("name", "detail"), sep = "-", extra="merge", fill="right") %>%
    mutate_at(c("name", "detail"), str_trim) %>%
    mutate(color = case_when(is.na(color) == TRUE ~detail,
                             TRUE ~color)) %>%
    mutate(across(.cols = contains("_id"), as.character))
  
  
  # handle three scenarios of product_id == "0" and/or variation_id == NA
  
  order_main_pivot <- order_main_pivot %>% 
    # [1] product_id != "0" & variation_id is.na indicated a simple product
    ## for simple products, variation_id will be replaced with "1111" to explicitly indicate simple products
    mutate(variation_id = case_when(is.na(variation_id)==TRUE & product_id !=0 ~ "1111",
                                    TRUE ~ as.character(variation_id))) %>%
    # [2] when there's a variation_id but no product_id:
    ## WooCommerce' mistake? the id listed as variation_id should actually be in the product_id column 
    ## comparing against wcSmartManager plugin shows this
    ## for these instances, swap the contents of product_id and variation_id
    mutate(product_id = case_when(product_id == "0" & is.na(variation_id) == FALSE ~ variation_id,
                                  TRUE ~ as.character(product_id)),
           # variation_id = "9999" indicates a missing variation_id as a result of issue [1]
           # this is likely due to data loss
           variation_id = case_when(product_id == variation_id ~"9999",
                                    TRUE ~ as.character(variation_id))
    ) %>%
    # [3] when product_id == "0" & variation_id is.na 
    ## these are products that have since been deleted
    ## these products will also be deleted from the database from this date on
    filter(product_id != "0" & is.na(variation_id)==FALSE)  
   
  
  #------------------
  # output message
  
  n_9999 <- order_main_pivot %>% filter(variation_id == "9999") %>% distinct(product_id) %>% nrow()
  n_1111 <- order_main_pivot %>% filter(variation_id == "1111") %>% distinct(order_id) %>% nrow()
  p_9999 <- as.integer(n_9999/length(unique(order_main_pivot$order_id))*100)
  cat(sprintf("%i products have been assigned a variation_id of \"1111\"indicating a SIMPLE PRODUCT\n", n_1111))
  cat(sprintf("%i orders (%i%% of all orders) in order_main_pivot have products that have been assigned a variation_id of \"9999\". Investigate if this gets too high.",n_9999, p_9999))
  
  
  #------------------
  
  data_norm$order_product <- order_main_pivot %>%
    group_by(order_id, product_id, variation_id) %>% 
    summarise("quantity" = sum(quantity), .groups = "keep") %>%
    as.data.frame()
  
  #------------------
  
  # unpack orders with multiple coupons
  coupon_list <- strsplit(order_main_pivot$coupon_items, split = ";")
  
  # create a new data frame with room to insert the coupons previously packed in comma-separated character strings
  data_norm$order_coupon <- data.frame(order_id = rep(order_main_pivot$order_id, sapply(coupon_list, length)), 
                                      "coupon" = unlist(coupon_list)) %>%
    mutate(coupon = str_sub(str_trim(str_extract(coupon, "(?<=code:).*(?=amount:)")), start=1, end=-2)) %>%
    filter(is.na(coupon)==FALSE) %>%
    distinct(order_id,
             coupon)
  
  #------------------
  
  # unpack meta.yarn_usage with multiple usages
  usage_list <- strsplit(order_main_pivot$meta.yarn_usage, split = ",")
  # create a new data frame with room to insert the usages previously packed in comma-separated character strings
  data_norm$order_usage <- data.frame(order_id = rep(order_main_pivot$order_id, sapply(usage_list, length)), 
                                        meta.yarn_usage = str_trim(unlist(usage_list))) %>%
    filter(is.na(meta.yarn_usage)==FALSE) %>%
    distinct()
  #------------------
  
  

  ###############
  # product_
  ###############
  
  data_norm$product <- order_main_pivot %>%
    group_by(product_id,
             variation_id) %>% 
    # get most recent data for a specific product_id / variation_id combo
    mutate("max_date_by_pid_vid" = max(order_date)) %>% 
    filter(max_date_by_pid_vid==order_date) %>% 
    # for orders on the same date/time with more than one color/name (usually because of 9999 variation_id)
    summarize(across(c(name, color), max), .groups = "keep") %>%
    ungroup()
  
  #------------------
  
  product_category <- df_list$product_main %>% 
    mutate("fiber" = str_extract_all(categories, "(?<=fiber > )[^,]*"), 
           "yarn_weight" = str_extract_all(categories, "(?<=yarn weight > )[^,]*"),
           "effect" = str_extract_all(categories, "(?<=effect > )[^,]*"),
           "hue" = str_extract_all(categories, "(?<=hue > )[^,]*"),
           "big_cones" = case_when(str_detect(categories, "big cones")==TRUE ~"big cones",
                                   TRUE ~ NA_character_),
           "spools" = case_when(str_detect(categories, "spools")==TRUE ~"spools",
                                TRUE ~ NA_character_),
           id = as.character(id)) %>%
    # categories are assigned at the product level
    filter(type != "variation")
  
  #------------------ 
  
  data_norm$product_fiber <- product_category %>%
    rename("product_id"=id) %>%
    unnest(fiber) %>% 
    select(product_id,
           fiber) 
  
  #------------------
  
  data_norm$product_yarn_weight <- product_category %>%
    rename("product_id"=id) %>% 
    unnest(yarn_weight) %>% 
    select(product_id,
           yarn_weight) 
  
  #------------------
  
  data_norm$product_effect <- product_category %>%
    rename("product_id"=id) %>%
    unnest(effect) %>% 
    select(product_id,
           effect)
  
  #------------------
  # usage_list is defined at data_norm$order_usage
  # create a new data frame with room to insert the usages previously packed in comma-separated character strings
  data_norm$product_usage <- data.frame(product_id = rep(order_main_pivot$product_id, sapply(usage_list, length)),
                                        meta.yarn_usage = unlist(usage_list)) %>%
    filter(is.na(meta.yarn_usage)==FALSE) %>%
    # how many times was each product used for a specific usage? 
    #*****caution - this is not really product-level data.****************
    # usage info is gathered at checkout (order-level data).
    # so, if a customer buys some yarn for knitting and some yarn for crocheting, this table wouldn't
    # know which was purchased for which. to account for that, a rule has been implemented.
    # a product must match a usage at least two times before it is recorded in this table****
    count(product_id, meta.yarn_usage) %>% 
    filter(n >= 2) %>%
    select (-n)
  
  #------------------
  
  # int -> char (id columns in df_list$product_hue)
  df_list$product_hue <- df_list$product_hue %>%
    mutate(across(.cols = contains("_id"), as.character)) 
  
  # join df_list$product_hue to data_norm$product
  ## in resulting data_norm$product_hue, hue will be NA for new products
  ## NAs must be filled manually assigning hue in excel by editing the ./data/product_hue.csv
  data_norm$product_hue <- data_norm$product %>% 
    left_join(df_list$product_hue,
              by = c("product_id", "variation_id")) %>%
    rename("name" = name.x,
         "color" = color.x) %>% 
    select(product_id, 
           variation_id,
           name,
           color,
           hue)  
  

  #------------------
  #outut message
  
  cat(sprintf("\n\n%i normalized data frames created:\n", length(data_norm)))
  cat(names(data_norm), sep="\n")
  return(data_norm)
}

# check primary keys
#-------------------------------------------------------------
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
    "pk_order_usage" = data_norm$order_usage %>%
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
    "pk_product_hue" = data_norm$product_hue %>%
      select(product_id,
             variation_id,
             hue), # hue is not really in the PK but i want to get a message if it's null so we can manually assign the hue
    "pk_product_usage" = data_norm$product_usage %>%
      select(everything())
    )
  
  return(df_list)
}


# check primary keys
#-------------------------------------------------------------

check_primary_keys <- function(df_list){
  
  # get list of primary key data frames
  pk_list <- df_list
  # [1] check each pk in pk_list for uniqueness, NAs, and empty strings. 
  ## record the results in criteria_df
  pk_check <- as.data.frame(
    cbind(is_unique = lapply(pk_list, function(pk){if_else(nrow(distinct(pk))==nrow(pk),
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
    cat("INVALID PRIMARY KEYS: \n The following dataframes have invalid primary keys:\n")
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


# denormalize data
#-------------------------------------------------------------

denormalize <- function(df_list){

  # [1] join order level data
  data_denorm <- df_list$order_product %>%
    left_join(df_list$order, by="order_id") %>%
    left_join(df_list$order_coupon, by="order_id") %>%
    left_join(df_list$order_usage, by="order_id") %>%
    # [2] join product data
    left_join(df_list$product, by=c("product_id"="product_id", "variation_id"="variation_id")) %>%
    left_join(df_list$product_hue, by=c("product_id"="product_id", "variation_id"="variation_id")) %>%
    left_join(df_list$product_fiber, by="product_id") %>%
    left_join(df_list$product_yarn_weight, by="product_id") %>%
    left_join(df_list$product_effect, by="product_id") %>%
    # handle duplicate fields
    rename("name"=name.x,
           "color"=color.x)%>%
    select(-c(name.y, color.y)) %>% 
    # adjust date & handle NAs for app
    mutate("order_year" = year(order_date),
           "order_month" = month(order_date),
           "order_week" = week(order_date),
           color = case_when(variation_id == "1111" ~"simple",
                             TRUE ~ color),
           #for these products, yarn_weight and fiber are irrelevant (ex. vintage textile bobbins). replace with "not applicable" so filtering works
           yarn_weight = case_when(product_id %in% c("2985", "19963", "19955", "3263", "1956") ~"not applicable",
                                   TRUE ~ yarn_weight),
           fiber = case_when(product_id %in% c("2985", "19963", "19955", "3263") ~"not applicable",
                             TRUE ~ fiber)) %>%
    # replace NA values in usage & effect with "unassigned" so that filtering will work
    mutate(across(c(meta.yarn_usage, effect), ~case_when(is.na(.x)==TRUE ~"unassigned",
                                                         TRUE ~ as.character(.x)))) %>%
    # handle strange missing data
    filter(is.na(quantity)==FALSE) 
  
  # return denormalized df
  return(data_denorm)
}

# wholesaler emails for mailchimp
#---------------------------------------------------------------------
get_wholesale_emails <- function(role_df, email_df){
  # this df contains user_ids of wholesale buyers
  wholesale_df <- data_norm$wholesale_df
  # this df contains emails and user_ids of all users
  email_df <- data$role_main
  # do an inner join to get emails of only wholesale buyers
  wholesale_emails <- email_df %>% 
    mutate(id = as.character(id)) %>%
    inner_join(wholesale_df, by = c("id"="user_id")) %>% 
    select(user.email)
  # return list of wholesaler emails
  return(wholesale_emails)
}

# export denormalized data
#-------------------------------------------------------------
export_data <- function(data_norm, data_denorm, data, wholesale_emails){

  # write denormalized data frame to the /data directory
  write.csv(data_denorm, "denormalized.csv", row.names = FALSE)
  cat("denoramlized dfs written to the data directory \n")
  
  # write wholesale_email_df to the /data directory
  write.csv(wholesale_emails, "wholesale_emails.csv", row.names = FALSE)
  cat("wholesale email df written to the data directory \n")
  
  # write product_hue data to the /data directory
  write.csv(data_norm[["product_hue"]], "product_hue.csv", row.names = FALSE)
  cat("product_hue.csv updated in the data directory \n")
  
  data_main <- names(data)[grepl("_main",names(data))]
  lapply(data_main, FUN = function(df){
    write.csv(data[[df]], paste(df,".csv",sep=""), row.names = FALSE)
  })
  cat("_main.csvs updated in the data directory \n")
  
}

"%notin%" <- Negate("%in%")



