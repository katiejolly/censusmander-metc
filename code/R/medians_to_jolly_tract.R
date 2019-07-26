library(tidycensus)

v10 <- load_variables(year = 2010, dataset = "acs5")
jolly_tract_geoids <- read_csv("jolly_tract_geoids.csv", 
                               col_types = cols(GEOID_BG = col_character(), 
                                                GEOID_TR = col_character(), RID = col_character()))


get_acs_metc <- function(table, year = 2017){
  reduce(
    map(c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"), function(x) {
      get_acs(geography = "block group", table = table, year = year, state = "MN", county = x)
    }), 
    rbind
  )
}

GRENT17 <- get_acs_metc(table = "B25063")

GRENT_names <- load_variables(year = 2017, dataset = "acs5") %>%
  filter(str_detect(name, "B25063"))


HVALUE17 <- get_acs_metc(table = "B25075")

HVALUE_names <- load_variables(year = 2017, dataset = "acs5") %>%
  filter(str_detect(name, "B25075"))

INCOME17 <- get_acs_metc(table = "B19001")

names <- load_variables(year = 2017, dataset = "acs5") %>%
  filter(str_detect(name, "B19001")) %>%
  bind_rows(GRENT_names) %>%
  bind_rows(HVALUE_names) %>%
  filter(!str_detect(name, "[A-Za-z]_"))
  

bg17data <- GRENT17 %>%
  bind_rows(HVALUE17) %>%
  bind_rows(INCOME17) %>%
  filter(!str_detect(NAME, "[A-Za-z]_")) %>% # take out the race specific tables
  select(-NAME) %>%
  left_join(names, by = c("variable" = "name")) %>%
  left_join(jolly_tract_geoids, by = c("GEOID" = "GEOID_BG")) %>%
  janitor::clean_names() %>%
  drop_na(rid) %>% # only in study area
  filter(rid != -999)


jt17data <- bg17data %>%
  group_by(rid, variable, label, concept) %>%
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe, estimate))

########## MEDIAN HOME VALUE

HVALJT17 <- jt17data %>%
  filter(concept == "VALUE", variable != "B25075_001") %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $10 000" ~ 10000,
    range == "$2 000 000 or more" ~ 2000000,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))

calculate_median_hhvalue17 <- function( geoid){
  if(!is.null(geoid)){
    ends <- HVALJT17 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- HVALJT17 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(HVALJT17, quantile)
    names(ends) <- pull(HVALJT17, endpoint)
  }
  for(i in 1:26){
    if(max(ends) < 1){
      return(tibble(rid = geoid, HHMEDVAL17 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, HHMEDVAL17 = med))
    }
    if(i < 26){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, HHMEDVAL17 = med))
      }else{
        next
      } 
    }
  }
}

geoids <- unique(HVALJT17$rid)

MEDHOMEVAL17 <- map_dfr(geoids, calculate_median_hhvalue17)


######## MEDIAN INCOME

INCJT17 <- jt17data %>%
  filter(concept == "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)", variable != "B19001_001") %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $10 000" ~ 10000,
    range == "$200 000 or more" ~ 200000,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))


calculate_median_income17 <- function( geoid){
  if(!is.null(geoid)){
    ends <- INCJT17 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- INCJT17 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(INCJT17, quantile)
    names(ends) <- pull(INCJT17, endpoint)
  }
  for(i in 1:16){
    if(max(ends) < 1){
      return(tibble(rid = geoid, HHMEDINC17 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, HHMEDINC17 = med))
    }
    if(i < 16){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, HHMEDINC17 = med))
      }else{
        next
      } 
    }
  }
}

MEDINCOME17 <- map_dfr(geoids, calculate_median_income17)


######### MEDIAN GROSS RENT

GRENTJT17 <- jt17data %>%
  filter(concept == "GROSS RENT", !variable %in% c("B25063_001", "B25063_002", "B25063_027")) %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!With cash rent!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $100" ~ 100,
    range == "$3 500 or more" ~ 3500,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))

calculate_median_rent17 <- function(geoid){
  if(!is.null(geoid)){
    ends <- GRENTJT17 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- GRENTJT17 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(GRENTJT17, quantile)
    names(ends) <- pull(GRENTJT17, endpoint)
  }
  for(i in 1:21){
    if(max(ends) < 1){
      return(tibble(rid = geoid, GROSSRENT17 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, GROSSRENT17 = med))
    }
    if(i < 21){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, GROSSRENT17 = med))
      }else{
        next
      } 
    }
  }
}

GROSSRENT17 <- map_dfr(geoids, calculate_median_rent17)

############ 2010

nhgis0002_ds176_20105_2010_blck_grp <- read_csv("code/metc_data/nhgis0002_ds176_20105_2010_blck_grp.csv") %>%
  filter(COUNTYA %in% c("003", "053", "123", "163", "019", "139", "037")) %>%
  mutate(GEOID = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))


nhgis_codes <- names %>%
  mutate(nhgis = case_when(
    str_detect(name, "B19001") ~ sub("B19001_", "JOH", name),
    str_detect(name, "B25063") ~ sub("B25063_", "JS4", name),
    str_detect(name, "B25075") ~ sub("B25075_", "JTG", name)
  ))

nhgis_data <- nhgis0002_ds176_20105_2010_blck_grp %>% select(GEOID, 37:169) %>%
  gather(key = "variable", value = "value", 2:134) %>%
  mutate(category = case_when(
    str_detect(variable, "M") ~ "moe",
    str_detect(variable, "E") ~ "estimate"
  )) %>%
  spread(key = category, value = value) %>%
  mutate(variable = str_remove(variable, "E|M")) %>%
  filter(variable != "NAE_M") %>%
  group_by(GEOID, variable) %>%
  summarise_all(min, na.rm = TRUE) %>%
  left_join(nhgis_codes, by = c("variable" = "nhgis")) %>%
  ungroup()

  

bg10data <- nhgis_data %>%
  left_join(jolly_tract_geoids, by = c("GEOID" = "GEOID_BG")) %>%
  janitor::clean_names() %>%
  drop_na(rid) %>% # only in study area
  filter(rid != -999)


jt10data <- bg10data %>%
  group_by(rid, variable, label, concept) %>%
  summarise(estimate = sum(as.numeric(estimate)),
            moe = moe_sum(as.numeric(moe), as.numeric(estimate)))


### Median values

calculate_median_hhvalue10 <- function( geoid){
  if(!is.null(geoid)){
    ends <- HVALJT10 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- HVALJT10 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(HVALJT10, quantile)
    names(ends) <- pull(HVALJT10, endpoint)
  }
  for(i in 1:26){
    if(max(ends) < 1){
      return(tibble(rid = geoid, HHMEDVAL10 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, HHMEDVAL10 = med))
    }
    if(i < 26){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, HHMEDVAL10 = med))
      }else{
        next
      } 
    }
  }
}

calculate_median_rent10 <- function(geoid){
  if(!is.null(geoid)){
    ends <- GRENTJT10 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- GRENTJT10 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(GRENTJT10, quantile)
    names(ends) <- pull(GRENTJT10, endpoint)
  }
  for(i in 1:21){
    if(max(ends) < 1){
      return(tibble(rid = geoid, GROSSRENT10 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, GROSSRENT10 = med))
    }
    if(i < 21){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, GROSSRENT10 = med))
      }else{
        next
      } 
    }
  }
}

calculate_median_income10 <- function( geoid){
  if(!is.null(geoid)){
    ends <- INCJT10 %>%
      filter(rid == geoid) %>%
      pull(quantile)
    # print(ends)
    names(ends) <- INCJT10 %>%
      filter(rid == geoid) %>%
      pull(endpoint)
    # print(names(ends))
  } else{
    ends <- pull(INCJT10, quantile)
    names(ends) <- pull(INCJT10, endpoint)
  }
  for(i in 1:16){
    if(max(ends) < 1){
      return(tibble(rid = geoid, HHMEDINC10 = 0))
      break
    }
    if(min(ends) == 1){
      med <- mean(c(as.numeric( names(ends)[1])), 0)
      return(tibble(rid = geoid, HHMEDINC10 = med))
    }
    if(i < 16){
      if(between(.5, ends[i], ends[i+1])){
        x <- c(ends[i], ends[i+1])
        med <- as.numeric((((.5 - x[1]) / (x[2] - x[1])) * (as.numeric(names(x)[2]) - as.numeric( names(x)[1]))) + as.numeric( names(x)[1]))
        return(tibble(rid = geoid, HHMEDINC10 = med))
      }else{
        next
      } 
    }
  }
}



GRENTJT10 <- jt10data %>%
  filter(concept == "GROSS RENT", !variable %in% c("JS4001", "JS4002", "JS4027")) %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!With cash rent!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $100" ~ 100,
    range == "$3 500 or more" ~ 3500,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))

geoids <- unique(jt10data$rid)

GROSSRENT10 <- map_dfr(geoids, calculate_median_rent10)


INCJT10 <- jt10data %>%
  filter(concept == "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)", variable != "JOH001") %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $10 000" ~ 10000,
    range == "$200 000 or more" ~ 200000,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))

MEDINCOME10 <- map_dfr(geoids, calculate_median_income10)


HVALJT10 <- jt10data %>%
  filter(concept == "VALUE", variable != "JTG001") %>%
  group_by(rid) %>%
  mutate(cumulative_units = cumsum(estimate)) %>%
  mutate(quantile = cumulative_units / sum(estimate)) %>%
  mutate(quantile = if_else(is.nan(quantile), 0, quantile)) %>%
  mutate(range = str_remove(label, "Estimate!!Total!!")) %>%
  mutate(endpoint = case_when(
    range == "Less than $10 000" ~ 10000,
    range == "$2 000 000 or more" ~ 2000000,
    TRUE ~ as.numeric(str_remove_all(sub('.*to', '', range),  '\\$| '))
  ))

MEDHOMEVAL10 <- map_dfr(geoids, calculate_median_hhvalue10)

medians_jollytracts00 <- read_csv("medians_jollytracts00.csv")

medians <- MEDHOMEVAL10 %>%
  full_join(MEDHOMEVAL17) %>%
  full_join(MEDINCOME10) %>%
  full_join(MEDINCOME17) %>%
  full_join(GROSSRENT10) %>%
  full_join(GROSSRENT17) %>%
  full_join(medians_jollytracts00 %>% mutate(rid = as.character(RID)) %>% select(-RID), by = c("rid" ))

write_csv(medians, "medians_jollytract.csv")
