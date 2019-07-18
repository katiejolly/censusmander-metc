library(tigris)
library(sf)
library(leaflet)

# musa <- st_read("code/output/shp/plan_musa_composite.gpkg") %>% # from MNGEO https://gisdata.mn.gov/dataset/us-mn-state-metc-plan-musa-composite
#   rename(geometry = geom) %>%
#   st_transform(26915)
# 
# 
# ggplot(musa) +
#   geom_sf() +
#   theme_void()
# 
# bg <- st_read("code/output/shp/tl_2017_27_bg.shp") %>%
#   st_transform(26915) 
# 

musa_geoids <- read_csv("code/output/shp/SNC_Tracts_Incl_Cities.csv")



bg_musa <- bg %>%
  mutate(tract = str_sub(GEOID, 1, 11)) %>%
  mutate(tract = as.numeric(tract)) %>%
  inner_join(musa_geoids, by = c("tract" = "GEOID")) %>%
  distinct(GEOID, .keep_all= TRUE)



st_write(bg_musa, "code/metc_data/target/bg_musa.shp")


# 
# ggplot(musa) +
#   geom_sf() +
#   theme_void()
# 
# ggplot(bg_musa %>% group_by(BENCHMARK)) +
#   geom_sf() +
#   theme_void()
# 
# 
# musa_bg_geoids <- unique(bg_musa$GEOID)



target_estimates_metc <- read_csv("code/metc_data/target/target_estimates_metc.csv") 

target_estimates_musa <- target_estimates_metc %>%
  mutate(tract = str_sub(id, 1, 11)) %>%
  inner_join(musa_geoids %>% mutate(GEOID = as.character(GEOID)), by = c("tract" = "GEOID")) %>%
  distinct(id, .keep_all= TRUE) %>%
  select(-tract)

write_csv(target_estimates_musa, "code/metc_data/target/target_estimates_musa.csv")

target_moe_metc <- read_csv("code/metc_data/target/target_moe_metc.csv")

target_moe_musa <- target_moe_metc %>%
  mutate(tract = str_sub(id, 1, 11)) %>%
  inner_join(musa_geoids %>% mutate(GEOID = as.character(GEOID)), by = c("tract" = "GEOID")) %>%
  distinct(id, .keep_all= TRUE) %>%
  select(-tract)

write_csv(target_moe_musa, "code/metc_data/target/target_moe_musa.csv")


target_population <- read_csv("code/metc_data/target/target_population.csv")

target_population_musa <- target_population %>%
  mutate(tract = str_sub(id, 1, 11)) %>%
  inner_join(musa_geoids %>% mutate(GEOID = as.character(GEOID)), by = c("tract" = "GEOID")) %>%
  distinct(id, .keep_all= TRUE) %>%
  select(-tract)

write_csv(target_population_musa, "code/metc_data/target/target_population_musa.csv")


target_estimates_musa_3var <- target_estimates_musa %>%
  select(-contains("65"))

write_csv(target_estimates_musa_3var, "code/metc_data/target/target_estimates_musa_3var.csv")

target_moe_musa_3var <- target_moe_musa %>%
  select(-contains("65"))

write_csv(target_moe_musa_3var, "code/metc_data/target/target_moe_musa_3var.csv")
