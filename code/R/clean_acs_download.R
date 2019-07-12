library(tidyverse)

################# AGE BREAKDOWN


sex_by_age_17 <- read_csv("code/metc_data/sex_by_age_17.csv", 
                          skip = 1) 


sex_by_age_long <- sex_by_age_17 %>%
  gather(key = "variable", value = "count", 4:101) %>%
  mutate(variable = gsub("[[:punct:]]", "", variable))


sex_by_age_est <- sex_by_age_long %>%
  filter(str_detect(variable, "Estimate")) %>%
  mutate(age_group = if_else(variable %in% c("Estimate Female  Under 5 years", "Estimate Male  Under 5 years", "Estimate Female  5 to 9 years", "Estimate Male  5 to 9 years", "Estimate Female  10 to 14 years", "Estimate Male  10 to 14 years", "Estimate Female  15 to 17 years", "Estimate Male  15 to 17 years"), 
                             "under18", 
                             if_else(variable %in% c("Estimate Male  65 and 66 years", "Estimate Male  67 to 69 years", "Estimate Male  70 to 74 years", "Estimate Male  75 to 79 years", "Estimate Male  80 to 84 years" , "Estimate Male  85 years and over", "Estimate Female  65 and 66 years", "Estimate Female  67 to 69 years", "Estimate Female  70 to 74 years", "Estimate Female  75 to 79 years", "Estimate Female  80 to 84 years" , "Estimate Female  85 years and over"), "over65", if_else(variable %in% c("Estimate Female", "Estimate Male", "Estimate Total"), "total", "working")))) %>%
  filter(age_group %in% c("under18", "over65") | variable == "Estimate Total") %>%
  group_by(Id2, age_group) %>%
  summarise(sumest = sum(count)) %>%
  spread(key = "age_group", value = "sumest") %>%
  select(id = Id2, under18, total18 = total, over65) %>%
  mutate(total65 = total18)


sex_by_age_moe <- sex_by_age_long %>%
  filter(str_detect(variable, "Margin")) %>%
  mutate(age_group = if_else(variable %in% c("Margin of Error Female  Under 5 years", "Margin of Error Male  Under 5 years", "Margin of Error Female  5 to 9 years", "Margin of Error Male  5 to 9 years", "Margin of Error Female  10 to 14 years", "Margin of Error Male  10 to 14 years", "Margin of Error Female  15 to 17 years", "Margin of Error Male  15 to 17 years"), 
                             "under18", 
                             if_else(variable %in% c("Margin of Error Male  65 and 66 years", "Margin of Error Male  67 to 69 years", "Margin of Error Male  70 to 74 years", "Margin of Error Male  75 to 79 years", "Margin of Error Male  80 to 84 years" , "Margin of Error Male  85 years and over", "Margin of Error Female  65 and 66 years", "Margin of Error Female  67 to 69 years", "Margin of Error Female  70 to 74 years", "Margin of Error Female  75 to 79 years", "Margin of Error Female  80 to 84 years" , "Margin of Error Female  85 years and over"), "over65", if_else(variable %in% c("Margin of Error Female", "Margin of Error Male", "Margin of Error Total"), "total", "working")))) %>%
  filter(age_group %in% c("under18", "over65") | variable == "Margin of Error Total") %>%
  group_by(Id2, age_group) %>%
  summarise(moe = moe_sum(count)) %>%
  spread(key = "age_group", value = "moe") %>%
  select(id = Id2, under18_moe = under18, total18_moe = total, over65_moe = over65) %>%
  mutate(total65_moe = total18_moe)


################### YEAR STRUCTURE BUILT

year_structure_built_17 <- read_csv("code/metc_data/year_structure_built_17.csv", 
                                    skip = 1) %>%
  select(1:11) 

year_structure_built_long <- year_structure_built_17 %>%
  gather(key = "variable", value = "count", 4:11) %>%
  mutate(variable = gsub("[[:punct:]]", "", variable))


year_structure_built_est <- year_structure_built_long %>%
  filter(grepl("Estimate", variable, ignore.case = TRUE)) %>%
  mutate(cat = if_else(variable == "Estimate Total", "totalhu", "built_after_2000")) %>%
  group_by(Id2, cat) %>%
  summarise(sumest = sum(count)) %>%
  spread(key = "cat", value = "sumest") %>%
  select(id = Id2, built_after_2000, totalhu)

year_structure_built_moe <- year_structure_built_long %>%
  filter(grepl("Margin", variable, ignore.case = TRUE)) %>%
  mutate(cat = if_else(variable == "Margin of Error Total", "totalhu_moe", "built_after_2000_moe")) %>%
  group_by(Id2, cat) %>%
  summarise(moe = moe_sum(count)) %>%
  spread(key = "cat", value = "moe") %>%
  select(id = Id2, built_after_2000_moe, totalhu_moe)


################# PERCENT WHITE


race_by_hispanic_origin_17 <- read_csv("code/metc_data/race_by_hispanic_origin_17.csv", 
                                       skip = 1) %>%
  select(1:5, 8, 9) %>%
  gather("variable", "count", 4:7) %>%
  mutate(variable = gsub("[[:punct:]]", "", variable))

race_est <- race_by_hispanic_origin_17 %>%
  filter(grepl("Estimate", variable, ignore.case = TRUE)) %>%
  mutate(cat = if_else(variable == "Estimate Total", "totalppl", "totalwhite")) %>%
  group_by(Id2, cat) %>%
  summarise(sumest = sum(count)) %>%
  spread(key = "cat", value = "sumest") %>%
  select(id = Id2, totalwhite, totalppl)

race_moe <- race_by_hispanic_origin_17 %>%
  filter(grepl("Margin", variable, ignore.case = TRUE)) %>%
  mutate(cat = if_else(variable == "Margin of Error Total", "totalppl_moe", "totalwhite_moe")) %>%
  group_by(Id2, cat) %>%
  summarise(moe = moe_sum(count)) %>%
  spread(key = "cat", value = "moe") %>%
  select(id = Id2, totalwhite_moe, totalppl_moe)


########### JOIN TOGETHER

target_estimates_metc <- race_est %>%
  left_join(sex_by_age_est) %>%
  left_join(year_structure_built_est)


target_moe_metc <- race_moe %>%
  left_join(sex_by_age_moe) %>%
  left_join(year_structure_built_moe)

write_csv(target_estimates_metc, "code/metc_data/target/target_estimates_metc.csv")
write_csv(target_moe_metc, "code/metc_data/target/target_moe_metc.csv")


####### BLOCK GROUPS

bg <- reduce(
  map(c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"), function(x) {
    tigris::block_groups(state = "MN", county = x)
  }), 
  rbind
)

bg <- st_as_sf(bg)

st_write(bg, "code/metc_data/target/bg_metc.shp")


############## POPULATION

population <- target_estimates_metc %>%
  select(id, totalppl)

write_csv(population, "code/metc_data/target/target_population.csv")
