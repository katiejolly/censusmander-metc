library(tidyverse)

sex_by_age_17 <- read_csv("code/metc_data/sex_by_age_17.csv", 
                          skip = 1) 


sex_by_age_long <- sex_by_age_17 %>%
  gather(key = "variable", value = "count", 4:101) %>%
  mutate(variable = gsub("[[:punct:]]", "", variable))


sex_by_age_est <- sex_by_age_long %>%
  filter(str_detect(variable, "Estimate")) %>%
  mutate(age_group = if_else(variable %in% c("Estimate Female  Under 5 years", "Estimate Male  Under 5 years", "Estimate Female  5 to 9 years", "Estimate Male  5 to 9 years", "Estimate Female  10 to 14 years", "Estimate Male  10 to 14 years", "Estimate Female  15 to 17 years", "Estimate Male  15 to 17 years"), 
                             "under18", 
                             if_else(variable %in% c("Estimate Male  65 and 66 years", "Estimate Male  67 to 69 years", "Estimate Male  70 to 74 years", "Estimate Male  75 to 79 years", "Estimate Male  80 to 84 years" , "Estimate Male  85 years and over", "Estimate Female  65 and 66 years", "Estimate Female  67 to 69 years", "Estimate Female  70 to 74 years", "Estimate Female  75 to 79 years", "Estimate Female  80 to 84 years" , "Estimate Female  85 years and over"), "over65", if_else(variable %in% c("Estimate Female", "Estimate Male", "Estimate Total"), "total", "working"))))


sex_by_age_moe <- sex_by_age_long %>%
  filter(str_detect(variable, "Margin"))

