# drop_cols <- function(df_list){
#   # split df_list into two lists & order lists alphabetically in prep to loop
#   ## cols: dfs of column names and boolean field of keep or not keep
#   cols_df_list <- df_list[grepl("_cols", names(df_list))] %>% sort_cols()
#   ## raw: raw dfs
#   raw_df_list <- df_list[grepl("_raw", names(df_list))] %>% sort_cols()
#   # filter and select cols_df_list
#   cols_df_list <- lapply(cols_df_list, 
#                          function(x)
#                            filter(x, keep==1) %>%
#                            select(variable))
#   # initialize list for loop
#   raw_drop_df_list <- list()
#   for(i in 1:length(raw_df_list)){
#     # define variables
#     ## column names to keep
#     cols <- cols_df_list[[i]]$variable
#     ## corresponding df
#     df <- raw_df_list[[i]]
#     # redefine df with only columns %in% cols
#     df <- df %>%
#       select(all_of(cols))
#     # add df to the list of processed dfs
#     raw_drop_df_list[[i]] <- df}
#   names(raw_drop_df_list) <- lapply(names(raw_df_list), function(x)paste(x, "_drop", sep=""))
#   #output messages
#   df_names <- names(raw_drop_df_list)
#   cat(sprintf("\n%i new data frames have been created\nwith only the necessary fields from the _raw data frames:
#               \n--------------------------------------
#               \n", length(raw_drop_df_list)))
#   cat(df_names, sep="\n")
#   return(raw_drop_df_list)
# }



# standardize item columns
cols <- lapply(1:200, FUN = function(x){
  x <- paste("line_item_",x,sep="")
})

data$order_raw <-add_column(data$order_raw, !!!cols[setdiff(names(cols), names(data$order_raw))])
  
  
  #data$order_raw %>% add_column(!!!cols[!names(cols) %in% names(.)])






