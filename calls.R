######################### COVID-19 ANALYTICS #############################
### FUNCTION CALLS SCRIPT ... SELECT EVERYTHING AND PRESS CTRL + ENTER ###
##########################################################################

source("COVID19.R")
source("xgboost.R")
source("notification_system.R")
library(xgboost)
library(tidyverse)
library(caret)
library(Matrix)
library(RcppRoll)
library(lubridate)
library(fastDummies)
library(jsonlite)
library(RJSONIO)
library(googleway)
library(fastDummies)
library(stringr)
library(caret)
library(lattice)
Sys.setlocale(category = "LC_ALL", locale = "C")
options(scipen = 100)

############################ ETL #########################################
##########################################################################

# make a unique NHS data set
total_data <-
  clean_data(
    "./NHS Pathways Covid-19 data 2020-08-18.csv",
    "111 Online Covid-19 data_2020-08-19.csv"
  )

# make a ccg data set data from ONS
ccg_tags <- get_ccg_data(total_data)

# create an ONS tags dictionary
tags_dictionary <- make_tags_dictionary(ccg_tags)

# dummify ONS tag data
tag_dummies <- make_tag_dummies(ccg_tags)

# make modelling data set
modelling_data <- make_modelling_data(total_data, tag_dummies)

# export data to local machine to use in BigQuery
export_dashboard_data(modelling_data, total_data, ccg_tags)

########################### MODELLING ###################################
#########################################################################

# make all necessary data sets
sets <- make_data_sets(modelling_data, total_data)

################### demographics
# estimate best params
demo_xgb_params <-
  best_xgb(data = sets$demo_xgb_data, yvar = "triagecount")

# fit model and get vars importance
demo_xgb <-
  fit_xgboost(data = demo_xgb_data,
              yvar = "triagecount",
              best_params = demo_xgb_params)

saveRDS(demo_xgb, "demo_xgb.RDS")

########################## socio
# estimate best params
socio_xgb_params <-
  best_xgb(data = sets$socio_xgb_data, yvar = "triagecount")

# fit model and get vars importance
socio_xgb <-
  fit_xgboost(
    data = sets$socio_xgb_data,
    yvar = "triagecount",
    best_params = socio_xgb_params
  )
saveRDS(socio_xgb, "socio_xgb.RDS")

########################### locality
# estimate best params
locality_xgb_params <-
  best_xgb(data = sets$locality_xgb_data, yvar = "triagecount")

# fit model and get vars importance
locality_xgb <-
  fit_xgboost(
    data = sets$locality_xgb_data,
    yvar = "triagecount",
    best_params = locality_xgb_params
  )
saveRDS(locality_xgb, "locality_xgb.RDS")

################################ ccg
# estimate best params
ccg_xgb_params <-
  best_xgb(data = sets$ccg_xgb_data, yvar = "triagecount")

# fit model and get vars importance
ccg_xgb <-
  fit_xgboost(data = sets$ccg_xgb_data,
              yvar = "triagecount",
              best_params = ccg_xgb_params)
saveRDS(ccg_xgb, "ccg_xgb.RDS")

################################ date

# estimate best params
forecast_xgb_params <-
  best_xgb(data = sets$forecast_xgb_data, yvar = "triagecount")

# fit model and get vars importance
forecast_xgb <-
  fit_xgboost(
    data = sets$forecast_xgb_data,
    yvar = "triagecount",
    best_params = forecast_xgb_params
  )
saveRDS(forecast_xgb, "forecast_xgb.RDS")

########################### notification system #########################
#########################################################################

# make address book using Google Places api
address_book <- make_address_book(ccg_tags)

# make data sets
notification_datasets <-
  make_comparison_datasets("2020-04-28", "2020-04-30")

# build a model on each data set
notification_xgbs <- xgbs_notification(notification_datasets)

# compare features from both models and prepare a message for each address within the ccg
notification_table <- make_notification_table(notification_xgbs)

write.table(notification_table, file = "notification_table.tsv", append = FALSE, sep = "\t", dec = ".",
            row.names = F, col.names = TRUE)
