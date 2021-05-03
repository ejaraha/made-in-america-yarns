
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
  
  data$order_main <- data$order_main %>%
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
  
  
  data_norm$order <- data$order_main %>% 
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
  
  order_main_pivot <- data$order_main %>%
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
  # output message
  
  n_9999 <- order_main_pivot %>% filter(variation_id == "9999") %>% nrow()
  n_0000 <- order_main_pivot %>% filter(variation_id == "0000") %>% nrow()
  p_9999 <- as.integer(n_9999/nrow(order_main_pivot)*100)
  cat(sprintf("%i rows in order_main_pivot have been assigned a variation_id of \"0000\"\nindicating a SIMPLE PRODUCT", n_0000))
  cat(sprintf("%i rows in order_main_pivot have been assigned a variation_id of \"9999\"\nThat's %i% of the rows\n*?*?*Investigate if this gets above 10%", n_9999, P_9999))
  
  
  #------------------
  ##############################################################################
  data_norm$order_product <- order_main_pivot %>%
    group_by(order_id, product_id, variation_id) %>% 
    summarise("quantity" = sum(quantity), .groups = "keep") %>%
    as.data.frame()
  ##############################################################################
  #------------------
  
  data_norm$order_coupon <- data$order_main %>%
    mutate("coupon" = str_sub(str_trim(str_extract(coupon_items, "(?<=code:).*(?=amount:)")), start=1, end=-2)) %>%
    filter(is.na(coupon)==FALSE) %>%
    select(order_id,
           coupon)
  
  ###############
  # product_
  ###############
  ##############################################################################
  data_norm$product <- order_main_pivot %>%
    group_by(product_id,
          variation_id) %>% 
    # get most recent data for a specific product_id / variation_id combo
    mutate("max_date_by_pid_vid" = max(order_date)) %>% 
    filter(max_date_by_pid_vid==order_date) %>% 
    # for orders on the same date/time with more than one color/name (usually because of 9999 variation_id)
    summarize(across(c(name, color), max), .groups = "keep")
  ##############################################################################
  #------------------
  
  product_category <- data$product_main %>% 
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
  ##############################################################################
  # int -> char (id columns in DATA$product_hue)
  data$product_hue <- data$product_hue %>%
    mutate(across(.cols = contains("_id"), as.character)) 
  
  # join DATA$product_hue to data_norm$product
  ## in resulting data_norm$product_hue, hue will be NA for new products
  ## NAs must be filled manually assigning hue in excel by editing the ./data/product_hue.csv
  data_norm$product_hue <- data_norm$product %>%
    left_join(data$product_hue,
              by = c("product_id", "variation_id")) %>%
    select(product_id, 
           variation_id,
           hue) 

  ##############################################################################
  
  ###############
  # customer_
  ###############
  ##############################################################################
  data_norm$customer <- data$order_main %>%
    group_by(customer_id) %>%
    # get most recent info for a specific customer_id
    mutate("max_date_by_cid"=max(order_date)) %>%
    filter(max_date_by_cid == order_date) %>%
    ungroup() %>%
    select(customer_id,
             billing_email,
             user_id) %>% glimpse()
  
  ##############################################################################
  #------------------
  ##############################################################################
  data_norm$customer_usage <- data.frame(customer_id = rep(order_main_pivot$customer_id, sapply(usage_list, length)), 
                                         meta.yarn_usage = unlist(usage_list)) %>%
    filter(is.na(meta.yarn_usage)==FALSE)
  ##############################################################################
  #------------------
  ##############################################################################
  # make a list of lists to unpack users with multiple roles
  role_list <- strsplit(data$role_main[["wp.capabilities"]],":true") 
  # remove anything that isn't a letter
  role_list <- lapply(role_list, FUN = function(x){str_replace_all(x,"[^a-z]", "")})
  # create a data frame with an user_id for each role
  data_norm$customer_role <- data.frame("user_id" = rep(data$role_main[["id"]], sapply(role_list, length)),
                                        "role" = unlist(role_list)) %>%
    filter(role != "") %>%
    select(user_id, role)
  ##############################################################################
  #------------------
  #outut message
  
  cat(sprintf("\n\n%i normalized data frames created:\n", length(data_norm)))
  cat(names(data_norm), sep="\n")
  return(data_norm)


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
    "pk_product_hue" = data_norm$product_hue %>%
      select(everything()),
    "pk_customer" = data_norm$customer %>%
      select(customer_id),
    "pk_customer_usage" = data_norm$customer_usage %>%
      select(everything()),
    "pk_customer_role" = data_norm$customer_role %>%
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
    cat("INVALID PRIMARY KEYS: \n The following dataframes have invalid primary keys:\n")
    result <- invalid
    # if not, all good!
  }else{result <- "All primary keys are valid (unique, no nulls)."}}
 
# export normalized data
#-------------------------------------------------------------
export_data_norm <- function(df_list){
  lapply(names(data), function(df){
    # write all normalized data frames to the /data/normalized directory
    wd_data_norm <- "C:/Users/sjara/git/made-in-america-yarns/data/normalized"
    write.csv(data[[df]], paste(wd_data_norm, "/", df, ".csv", sep=""), row.names = FALSE)
  })
  
  cat("Noramlized tables have been written to the data/NORMALIZED directory \n")
  
  # write product_hue data to the /data directory AS WELL AS the /data/normalized directory
  write.csv(df_list[["product_hue"]], "product_hue.csv", row.names = FALSE)
  
  cat("Product_hue.csv has been updated in the /DATA directory \n")
}


