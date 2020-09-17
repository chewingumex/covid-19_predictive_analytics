####################################### XGB PARAMS FUNCTIONS
############################################################

# estimate best params
######################

best_xgb <- function(data, yvar) {
  best_param = list()
  best_seednumber = 1234
  best_rmse = 100000
  best_rmse_index = 0
  
  xgbDmatrix <- data %>%
    select(!yvar) %>%
    as.matrix() %>%
    xgb.DMatrix(data = .,
                label = unlist(data[yvar]))
  
  for (iter in 1:500) {
    param <- list(
      booster = "gblinear",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = sample(6:10, 1),
      eta = runif(1, .01, .7),
      gamma = runif(1, 0, 5),
      subsample = runif(1, .6, .9),
      colsample_bytree = runif(1, .5, .8),
      min_child_weight = sample(1:40, 1),
      max_delta_step = sample(1:10, 1),
      nthread = 6,
      lambda = runif(1, 0, 1),
      alpha = runif(1, 0, 1)
    )
    cv.nround = 1000
    seed.number = sample.int(10000, 1)[[1]]
    set.seed(seed.number)
    
    xgb <- xgb.cv(
      data = xgbDmatrix,
      params = param,
      nfold = 2,
      verbose = F,
      early_stopping_rounds = 5,
      maximize = F,
      nrounds = cv.nround,
      showsd = T,
      print_every_n = 10
    )
    
    min_rmse = xgb$evaluation_log[xgb$best_iteration]$test_rmse_mean
    min_rmse_index = xgb$best_iteration
    
    if (min_rmse < best_rmse) {
      best_rmse = min_rmse
      best_rmse_index = min_rmse_index
      best_seednumber = seed.number
      best_param = param
    }
    message(paste(
      "Iteration #",
      iter,
      "rmse =",
      min_rmse,
      "best rmse so far",
      best_rmse
    ))
    
  }
  
  best_xgb_params <- list(
    best_rmse = best_rmse,
    best_rmse_index = best_rmse_index,
    best_seednumber = best_seednumber,
    best_param = best_param
  )
  
  return(best_xgb_params)
}

# fit and evaluation function
#############################

fit_xgboost <- function(data, yvar, best_params) {
  set.seed(best_params$best_seednumber)
  
  dtrain <-
    data[createDataPartition(
      unlist(data[yvar]),
      times = 1,
      p = 0.75,
      list = T,
      groups = 5
    )$Resample1,]
  dtrain <- dtrain %>%
    select(!yvar) %>%
    as.matrix() %>%
    xgb.DMatrix(data = .,
                label = unlist(dtrain[yvar]))
  
  dtest <-
    data[!1:nrow(data) %in% createDataPartition(
      unlist(data[yvar]),
      times = 1,
      p = 0.75,
      list = T,
      groups = 5
    )$Resample1,]
  dtest <- dtest %>%
    select(!yvar) %>%
    as.matrix() %>%
    xgb.DMatrix(data = .,
                label = unlist(dtest[yvar]))
  
  xgb <-
    xgb.train(
      data = dtrain,
      params = best_params$best_param,
      nrounds = best_params$best_rmse_index,
      watchlist = list(train = dtrain, test = dtest)
    )
  
  importance_matrix = xgb.importance(feature_names = xgb$feature_names,
                                     model = xgb)
  
  if (identical(data, socio_xgb_data)) {
    importance_matrix <- importance_matrix %>%
      mutate(Feature = gsub("code_", "", .$Feature)) %>%
      mutate(Feature = gsub("super_7", "", .$Feature)) %>%
      left_join(., tags_dictionary, by = c("Feature" = "code")) %>%
      mutate(Feature = paste(supergroup, "-", group, "-", subgroup)) %>%
      select(-supergroup,-group,-subgroup) %>%
      filter(!Feature == "NA - NA - NA") %>%
      mutate(Weight = Weight * -1)
  }
  
  if (identical(data, locality_xgb_data)) {
    importance_matrix <- importance_matrix %>%
      mutate(Feature = gsub("locality_code_", "", .$Feature)) %>%
      left_join(., tags_dictionary, by = c("Feature" = "code")) %>%
      mutate(Feature = supergroup) %>%
      select(-supergroup,-group,-subgroup)
  }
  
  if(identical(data, ccg_xgb_data)) {
    importance_matrix <- importance_matrix %>% 
      mutate(Feature = gsub("ccgcode_","",.$Feature)) %>% 
      left_join(., tags_dictionary, by = c("Feature" = "code")) %>% 
      mutate(Feature = supergroup) %>%
      select(-supergroup,-group,-subgroup) %>% 
      mutate(Weight = Weight * -1)
  }
  
  if(identical(data,forecast_xgb_data)){
    importance_matrix <- xgb.importance(feature_names = xgb$feature_names,
                                        model = xgb)
    
  }
  
  xgb_plot <- xgb.ggplot.importance(
    importance_matrix = importance_matrix,
    top_n = if_else(
      identical(data, socio_xgb_data),
      20,
      if_else(identical(data, ccg_xgb_data), 
              30,
              if_else(identical(data,forecast_xgb_data),14,
              100))),
    rel_to_first = T,
    n_clusters = 3
  ) + theme(text = element_text(size = 22))
  
  return(results = list(xgb = xgb,
                        xgb_plot = xgb_plot))
  
}

make_data_sets <- function(modelling_data,total_data){
  
  demo_xgb_data <- modelling_data %>%
  select(triagecount, starts_with("sex"), starts_with("age")) %>%
  select(!sex_Unknown) %>%
  select(!age_NA) %>%
  aggregate(. ~ triagecount, data = ., sum) %>%
  tibble

socio_xgb_data <- modelling_data %>%
  select(triagecount,
         starts_with("code_"),
         starts_with("super")) %>%
  select(!code_9Z9) %>%
  aggregate(. ~ triagecount, data = ., sum) %>%
  tibble

locality_xgb_data <- modelling_data %>%
  select(triagecount,
         starts_with("locality")) %>%
  select(!contains("z")) %>%
  aggregate(. ~ triagecount, data = ., sum) %>%
  tibble

ccg_xgb_data <- modelling_data %>%
  select(triagecount,
         starts_with("ccg")) %>%
  aggregate(. ~ triagecount, data = ., sum) %>%
  tibble

forecast_xgb_data <- total_data %>% select(date,triagecount) %>% 
  aggregate(data =., triagecount ~ date, sum) %>%
  mutate(date = as_date(date)) %>%
  mutate(day = day(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(weekday = weekdays(date,abbreviate = F)) %>% 
  tibble %>% 
  mutate(lag_1 = lag(triagecount,1),
         avg_7 = lag(roll_meanr(triagecount, 7), 1),
         avg_3 = lag(roll_meanr(triagecount, 3), 1),
         avg_15 = lag(roll_meanr(triagecount, 15), 1),
         avg_30 = lag(roll_meanr(triagecount, 30), 1)
  ) %>% 
  drop_na(avg_30) %>% 
  mutate(across(c(lag_1,avg_7, avg_3,avg_15,avg_30), scale)) %>% 
  select(-date) %>% 
  dummy_cols(., select_columns = "weekday", remove_selected_columns = T)

  return(sets <- list(demo_xgb_data = demo_xgb_data,
    socio_xgb_data = socio_xgb_data,
    locality_xgb_data = locality_xgb_data,
    ccg_xgb_data = ccg_xgb_data,
    forecast_xgb_data = forecast_xgb_data))

}

