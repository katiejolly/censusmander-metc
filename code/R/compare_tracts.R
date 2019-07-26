cities <- st_read("code/metc_data/target/bdry_mn_city_township_unorg.gpkg")  %>%
  st_transform(26915)


msp <- cities %>%
  filter(FEATURE_NA %in% c("Saint Paul", "Minneapolis")) %>%
  st_transform(26915)


suburban_jt <- st_intersects(aggregate_rids_musa %>% st_transform(26915), msp)

suburban_jt[3]

suburban <- rep(0, 715)

for (i in 1:length(suburban_jt)){
  suburban[[i]] = suburban_jt[[i]]
}

jt_logical = lengths(suburban_jt) > 0
urban = aggregate_rids_musa[jt_logical, ]


suburban_tract <- st_intersects(tract %>% st_transform(26915), msp)

tract_logical = lengths(suburban_tract) > 0
urbantract = tract[tract_logical, ]


bloomington <- cities %>%
  filter(FEATURE_NA == "Bloomington")

bloomington_jt <- st_intersection(aggregate_rids_musa, bloomington) %>%
  mutate(area = st_area(.)) %>% filter(as.numeric(area) > 46832)

ggplot(bloomington_jt) + geom_sf() + geom_sf_label(aes(label = rids)) 



bloomington_tr <- st_intersection(tract, bloomington) %>%
  mutate(area = st_area(.)) %>% filter(as.numeric(area) > 46832)

ggplot(bloomington_tr) + geom_sf() + geom_sf_label(aes(label = GEOID)) 

bloom_tract <- st_intersects(tract %>% st_transform(26915), bloomington)

tract_logical = lengths(bloomington_tr) > 0
bloomtract = tract[tract_logical, ]

leaflet(bloomington_jt %>% st_transform(4326)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()

leaf.tr <- leaflet(tract %>% st_transform(4326)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolylines(color = "black")
