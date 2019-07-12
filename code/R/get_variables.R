library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(furrr)
library(tidycensus)

options(tigris_use_cache = TRUE)

plan(multisession)

v17 <- load_variables(year= "2017", dataset = "acs5")

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

get_metc_acs <- function(geography = "block group", state = "MN", counties = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"), table, year = "2017"){
  reduce(
    map(counties, function(x) {
      get_acs(geography = geography, table = table, 
              state = state, cache_table = TRUE)
    }), 
    rbind
  )
}

age <- get_metc_acs(table = "B01001")
