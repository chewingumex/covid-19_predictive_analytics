# read in NHS data
clean_data <- function(pathways, online) {
  pathways <-
    read_delim(pathways, delim = ",")
  online_111 <-
    read_delim(online, delim = ",")
  
  # cleaning data sets
  names(pathways) <- tolower(names(pathways))
  names(online_111) <- tolower(names(online_111))
  names(pathways)[2] <- "date"
  names(online_111)[1] <- "date"
  pathways$date <-
    as_date(strptime(pathways$date, format = "%d/%m/%Y"))
  online_111$date <-
    as_date(strptime(online_111$date, format = "%d/%m/%Y"))
  online_111$sitetype = "online"
  online_111 <- online_111 %>% rename(., triagecount = total)
  total_data <- rbind(online_111, pathways)
  total_data$ageband <-
    recode(total_data$ageband, "70-120 years" = "70+ years")
  
  return(total_data)
}

# retrieve CCG data // manually exclude buggy data

get_ccg_data <- function(total_data) {
  CCGs <- unique(total_data$ccgcode)
  CCGs <- CCGs[-c(192, 217, 219, 221, 235)]
  
   for (i in CCGs) {
     json <-
       jsonlite::fromJSON(paste0("https://findthatpostcode.uk/areas/", i, ".json"))
     write_json(json, paste0("./CCGDATA/", i))
   }
  
  # load jsons into a list
  jsonlist <- list.files("./CCGDATA/")
  jsons <- list()
  for (i in 1:length(jsonlist)) {
    jsons[[i]] <- RJSONIO::fromJSON(paste0("./CCGDATA/", jsonlist[i]))
  }
  
  # extract info from json files
  ccg_tags <- tibble(ccg = jsonlist)
  c = 1
  for (j in 1:length(jsons)) {
    for (i in 1:length(jsons[[j]]$included)) {
      ccg_tags[c, 1] = jsonlist[j]
      ccg_tags[c, 2] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$oac11[1]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$oac11[1])
      )
      ccg_tags[c, 3] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$oac11[2]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$oac11[2])
      )
      ccg_tags[c, 4] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$oac11[3]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$oac11[3])
      )
      ccg_tags[c, 5] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$oac11[4]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$oac11[4])
      )
      ccg_tags[c, 6] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$location[1]),
        0,
        unlist(jsons[[j]]$included[[i]]$attributes$location[1])
      )
      ccg_tags[c, 7] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$location[2]),
        0,
        unlist(jsons[[j]]$included[[i]]$attributes$location[2])
      )
      ccg_tags[c, 8] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$ru11ind[1]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$ru11ind[1])
      )
      ccg_tags[c, 9] = if_else(
        is.null(jsons[[j]]$included[[i]]$attributes$ru11ind[2]),
        "",
        unlist(jsons[[j]]$included[[i]]$attributes$ru11ind[2])
      )
      ccg_tags[c, 10] = if_else(
        is.null(jsons[[j]]$data$attributes$name),
        "",
        unlist(jsons[[j]]$data$attributes$name)
      )
      c = c + 1
    }
  }
  
  # fix names
  
  names(ccg_tags)[2:7] <-
    c(names(jsons[[1]]$included[[5]]$attributes$oac11), "lat", "lon")
  ccg_tags <- ccg_tags %>%
    drop_na(code) %>%
    mutate(super_code = str_extract(code, "^.{1}")) %>%
    drop_na(super_code) %>%
    rename(
      "locality_code" = "...8",
      "locality" = "...9",
      "ccg_name" = "...10"
    )
  
  # fix locality codes
  
  for (i in 1:nrow(ccg_tags)) {
    if (ccg_tags$locality_code[i] == 1) {
      ccg_tags$locality_code[i] <- "A1"
      ccg_tags$locality[i] <- "Urban major conurbation"
    }
    if (ccg_tags$locality_code[i] == 2) {
      ccg_tags$locality_code[i] <- "C1"
      ccg_tags$locality[i] <- "Urban city and town"
    }
    if (ccg_tags$locality_code[i] == 3) {
      ccg_tags$locality_code[i] <- "B1"
      ccg_tags$locality[i] <- "Urban minor conurbation"
    }
    if (ccg_tags$locality_code[i] == 4) {
      ccg_tags$locality_code[i] <- "D1"
      ccg_tags$locality[i] <- "Rural town and fringe"
    }
    if (ccg_tags$locality_code[i] == 5) {
      ccg_tags$locality_code[i] <- "D2"
      ccg_tags$locality[i] <-
        "Rural town and fringe in a sparse setting"
    }
    if (ccg_tags$locality_code[i] == 6) {
      ccg_tags$locality_code[i] <- "E1"
      ccg_tags$locality[i] <- "Rural village"
    }
    if (ccg_tags$locality_code[i] == 7) {
      ccg_tags$locality_code[i] <- "E2"
      ccg_tags$locality[i] <- "Rural village in a sparse setting"
    }
    if (ccg_tags$locality_code[i] == 8) {
      ccg_tags$locality_code[i] <- "F2"
      ccg_tags$locality[i] <-
        "Rural hamlet and isolated dwellings in a sparse setting"
    }
  }
  return(ccg_tags)
}



# create separate dictionaries, stack them

make_tags_dictionary <- function(ccg_tags) {
  super_code_tags <- ccg_tags %>%
    select(super_code, supergroup) %>%
    distinct %>%
    mutate(group = NA) %>%
    mutate(subgroup = NA) %>%
    rename("code" = "super_code")
  
  code_tags <- ccg_tags %>%
    select(code, group, subgroup, supergroup) %>%
    distinct
  
  locality_tags <- ccg_tags %>%
    select(locality_code, locality) %>%
    rename("code" = "locality_code", "supergroup" = "locality") %>%
    mutate(group = NA) %>%
    mutate(subgroup = NA)
  
  names_dictionary <- ccg_tags %>%
    select(ccg, ccg_name) %>%
    mutate(group = NA) %>%
    mutate(subgroup = NA) %>%
    rename("code" = "ccg", "supergroup" = "ccg_name") %>%
    distinct
  
  tags_dictionary <-
    bind_rows(super_code_tags, code_tags, locality_tags, names_dictionary) %>%
    distinct
  
  return(tags_dictionary)
  
}

# dumify and prepare ONS data

make_tag_dummies <- function(ccg_tags) {
  tag_dummies <- ccg_tags %>%
    select(ccg, code, super_code, locality_code) %>%
    dummy_cols(
      .,
      select_columns = c("code", "super_code", "locality_code"),
      remove_selected_columns = T
    ) %>%
    group_by(ccg) %>%
    summarise_all(sum)
  
  tag_dummies <- tag_dummies %>%
    select(starts_with(c("code_", "super", "local"))) %>%
    transmute_all(., function(x)
      dplyr::if_else(x > 1, 1, 0)) %>%
    transmute_all(., function(x)
      dplyr::if_else(is.na(x), 0, x)) %>%
    tibble(ccg = tag_dummies$ccg,
           .)
  return(tag_dummies)
}

make_modelling_data <- function(total_data, tag_dummies) {
  # join ONS and NHS data
  modelling_data <- total_data %>%
    ungroup %>%
    select(sex, ageband, ccgcode, triagecount, sitetype) %>%
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
               remove_selected_columns = T)
  
  names(modelling_data)[2:5] <-
    c("age_0_18", "age_19_69", "age_70_plus", "age_NA")
  
  return(modelling_data)
}

# data for dashboard

export_dashboard_data <-
  function(modelling_data, total_data, ccg_tags) {
    dashboard_data <- modelling_data
    write.csv(
      dashboard_data,
      "dashboard_data.csv",
      row.names = F,
      quote =  F,
      fileEncoding = "UTF-8"
    )
    
    dashboard_data2 <-
      as_tibble(apply(total_data, 2, function(x)
        gsub(",", "", x)))
    dashboard_data2$triagecount <-
      as.numeric(gsub("  ", "", dashboard_data2$triagecount))
    
    write.csv(
      dashboard_data2,
      "dashboard_data2.csv",
      row.names = F,
      quote =  F,
      fileEncoding = "UTF-8"
    )
    
    dashboard_data3 <- ccg_tags
    dashboard_data3 <-
      as_tibble(apply(dashboard_data3, 2, function(x)
        gsub(",", "", x)))
    
    write.csv(
      dashboard_data3,
      "dashboard_data3.csv",
      row.names = F,
      quote =  F,
      fileEncoding = "UTF-8"
    )
    
    return("Data was exported to the local direcotry and is ready for uploading to BigQuery.")
    
  }
