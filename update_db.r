library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
setwd("C:/Users/sjara/git/made-in-america-yarns/data")

# update data
#-------------------------------------------------------------
data <- update()

# normalize data
#-------------------------------------------------------------
data_norm <- normalize(data)

# check primary keys
#-------------------------------------------------------------
check_primary_keys(primary_key_list())
#lapply(data, check_empty)

# denormalize data
#-------------------------------------------------------------




# export denormalized tables
#-------------------------------------------------------------
# export_data_norm(data_norm)

