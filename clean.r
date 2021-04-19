library(dplyr)
library(tidyr)
library(lubridate)

source("C:/Users/sjara/git/made-in-america-yarns/functions.R")
wd <- "C:/Users/sjara/git/made-in-america-yarns/data"

# import data
#--------------------------------------------------------------

# import data as a list
data <- import_new_data()

# extract data frames from list
order <- as.data.frame(data[[1]])
product <- as.data.frame(data[[2]])


# clean data
#--------------------------------------------------------------

order <- clean_order()
product <- clean_product()





