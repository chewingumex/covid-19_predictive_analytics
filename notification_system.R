#### create notification address book

make_address_book <- (ccg_tags) {
  api_key <- read_json("apikey")
  notification_address_book <- data.frame()
  for (i in 1:nrow(ccg_tags)) {
    points <- c(ccg_tags$lat[i], ccg_tags$lon[i])
    query <-
      google_places(
        search_string = "*",
        location = points,
        radius = 500,
        key = api_key$apikey$key
      )$results
    message("Retrieved Google metadata for CCG #", i)
    address_book_segment <-
      cbind(query[c("name", "formatted_address", "types")], ccg_tags$ccg[i])
    notification_address_book <-
      rbind(address_book_segment, notification_address_book)
  }
  notification_address_book <- tibble(notification_address_book)
  names(notification_address_book)[4] <- "ccg"
  saveRDS(notification_address_book,
          "notification_address_book.RDS")
  return(notification_address_book)
}


make_notification_datasets <- function(date1, date2) {
  ccg_comp1 <- total_data %>%
    ungroup %>%
    select(sex, ageband, ccgcode, triagecount, sitetype, date) %>%
    filter(., (date == lubridate::as_date(date1))) %>%
    select(-date)
  
  ccg_comp2 <- total_data %>%
    ungroup %>%
    select(sex, ageband, ccgcode, triagecount, sitetype, date) %>%
    filter(., (date == lubridate::as_date(date2))) %>%
    select(-date)
  
  
  ccg_joined_list <- inner_join(ccg_comp1,
                                ccg_comp2,
                                by = c("ccgcode")) %>%
    select(ccgcode) %>%
    unique
  
  ccg_date1_xgb_data <- ccg_comp1 %>%
    filter(., ccgcode %in% ccg_joined_list$ccgcode) %>%
    dummy_cols(
      .,
      select_columns = c("ageband", "sex", "sitetype"),
      remove_selected_columns = T
    ) %>%
    inner_join(.,
               tag_dummies,
               by = c("ccgcode" = "ccg"),
               na_matches = "never") %>%
    drop_na(`ageband_0-18 years`) %>%
    dummy_cols(.,
               select_columns = "ccgcode",
               remove_selected_columns = T) %>%
    select(triagecount,
           starts_with("ccg"))  %>%
    aggregate(. ~ triagecount, data = ., sum) %>%
    tibble
  
  ccg_date2_xgb_data <- ccg_comp2 %>%
    filter(., ccgcode %in% ccg_joined_list$ccgcode) %>%
    dummy_cols(
      .,
      select_columns = c("ageband", "sex", "sitetype"),
      remove_selected_columns = T
    ) %>%
    inner_join(.,
               tag_dummies,
               by = c("ccgcode" = "ccg"),
               na_matches = "never") %>%
    drop_na(`ageband_0-18 years`) %>%
    dummy_cols(.,
               select_columns = "ccgcode",
               remove_selected_columns = T) %>%
    select(triagecount,
           starts_with("ccg"))  %>%
    aggregate(. ~ triagecount, data = ., sum) %>%
    tibble
  
  return(
    comparison_datasets <- list(
      ccg_date1_xgb_data = ccg_date1_xgb_data,
      ccg_date2_xgb_data = ccg_date2_xgb_data
    )
  )
}

# build ccg notification xgboosts

xgbs_notification <- function(notification_datasets) {
  best_notification_xgb_params <- list()
  for (i in 1:length(notification_datasets)) {
    best_notification_xgb_params[[i]] <-
      best_xgb(data = notification_datasets[[i]], yvar = "triagecount")
  }
  
  notification_xgb <- list()
  for (i in 1:length(best_notification_xgb_params)) {
    notification_xgb[[i]] <-
      fit_xgboost(data = notification_datasets[[i]],
                  yvar = "triagecount",
                  best_params = best_notification_xgb_params[[i]])
    saveRDS(notification_xgb[[i]], paste("notification_xgb_", i, ".RDS"))
  }
  return(notification_xgb)
}

# notification table

make_notification_table <- function(notification_xgbs) {
  importance_matrices <- list()
  notification_table <- tibble
  for (i in 1:length(notification_xgbs)) {
    importance_matrices[[i]] = xgb.importance(feature_names = notification_xgbs[[i]]$xgb$feature_names,
                                              model = notification_xgbs[[i]]$xgb)
  }
  notification_table <-
    inner_join(
      importance_matrices[[1]],
      importance_matrices[[2]],
      by = "Feature",
      suffix = c("_date1", "_date2")
    ) %>%
    tibble %>%
    mutate(Feature = gsub("ccgcode_", "", .$Feature)) %>%
    mutate(magnitude = abs(Weight_date1 - Weight_date2)) %>%
    mutate(direction = if_else(Weight_date2 > Weight_date1, "Increasing", "Decreasing")) %>%
    mutate(
      shift = case_when(
        Weight_date1 <= 0 & Weight_date2 <= 0 ~ "no_new_cases",
        Weight_date1 < 0 &
          Weight_date2 >= 0 ~ "surge_of_cases",
        Weight_date1 > 0 &
          Weight_date2 > 0 ~ "surge_of_cases_continues",
        Weight_date1 > 0 &
          Weight_date2 <= 0 ~ "surge_of_cases_ceased"
      )
    ) %>%
    mutate(
      message1 = if_else(
        direction == "Increasing",
        "The tendency in the local area is an increase of COVID-19 new cases.",
        "The tendency in the local area is a decrease of COVID-19 new cases."
      )
    ) %>%
    mutate(
      message2 = if_else(
        shift == "no_new_cases",
        " Right now there are no new cases in the local area.",
        if_else(
          shift == "surge_of_cases",
          " Right now there is a surge of new cases in the local area.",
          if_else(
            shift == "surge_of_cases_continues",
            " Right now the surge of cases continues in the local area.",
            if_else(
              shift == "surge_of_cases_ceased",
              " Right now the surge of cases ceased in the local area.",
              ""
            )
          )
        )
      )
    ) %>%
    mutate(message = paste(message1, message2)) %>%
    select(-message1, -message2) %>%
    left_join(., address_book, by = c("Feature" = "ccg"))
  
  return(notification_table)
}