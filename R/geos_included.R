# I do want to include the entire geography of ctus on the border (ie northfield, etc) 

########
# get full geography of cities which have some land inside the 7 county area
#########
county_list <- list(
  "Anoka County" = "003", # "Anoka",
  "Carver County" = "019", # "Carver",
  "Dakota County" = "037", # "Dakota",
  "Hennepin County" = "053", # "Hennepin",
  "Ramsey County" = "123", # "Ramsey",
  "Scott County" = "139", # "Scott",
  "Washington County" = "163", # "Washington"
  "Sherburne County" = "141", # "Sherburne",
  "Wright County" = "171", # "Wright"
  "Goodhue County" = "049",
  "Rice County" = "131",
  "Sibley County" = "143",
  "McLeod County" = "085",
  "Isanti County" = "059",
  "Chisago County" = "025",
  "Le Sueur County" = "079"
)

cities_geo <- tigris::county_subdivisions(
  state = 27,
  county = county_list,
  class = "sf"
) %>%
  dplyr::mutate(
    NAME = dplyr::case_when(
      LSAD == 44 ~ paste(NAME, "Twp."),
      LSAD == 46 ~ paste(NAME, "(unorg.)"),
      TRUE ~ NAME
    )
  ) %>%
  dplyr::left_join(mn_fips_codes,
                   by = c(
                     "COUNTYFP" = "county_code",
                     "STATEFP" = "state_code"
                   )
  )

cities <- cities_geo %>%
  dplyr::group_by(NAME) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::mutate(CTU_NAME = dplyr::if_else(
    n > 1 & LSAD != 25,
    paste0(NAME, " - ", county, " Co."), # cities dont get merged
    NAME
  )) %>%
  dplyr::group_by(CTU_NAME) %>%
  dplyr::summarise(
    geometry = sf::st_union(geometry),
    ALAND = sum(ALAND, na.rm = TRUE),
    AWATER = sum(AWATER, na.rm = TRUE)
  ) %>%
  dplyr::arrange(CTU_NAME)


# metc_inclusive <- fetch_ctu_geo(core = FALSE)
metc <- fetch_ctu_geo(core = TRUE)


mapview::mapview(cities)

metc_plus <- filter(cities, CTU_NAME %in% c(metc$CTU_NAME))
metc_plus %>%
  filter(CTU_NAME == "Northfield")

ggplot() +
  geom_sf(data = metc_plus, fill = "grey90") +
  theme_void() 

########
# intersect the city geographies with census tracts
# get a list of "additional" tracts that need to be pulled
#######
block_groups <- tigris::block_groups(state = 27,
               county = county_list)

intersecting_bgs<- block_groups %>%
  select(COUNTYFP, GEOID, ALAND, AWATER) %>%
  st_transform(26915) %>%
  st_intersection(metc_plus %>%
                    select(-ALAND, -AWATER) %>%
                    st_transform(26915) %>%
                    st_buffer(-100)) %>%
  mutate(area = as.numeric(st_area(.)))

parea_intersecting_bgs <- intersecting_bgs %>%
  filter(COUNTYFP %not_in% c("003", "019", "037", "053", "123", "139", "163")) %>%
  mutate(percent = area / (ALAND + AWATER)) %>%
  filter(percent > .05) #bg has to have more than 5% overlapping to be included
  
extra_bgs <- filter(block_groups,
                    COUNTYFP %in% c("003", "019", "037", "053", "123", "139", "163") |
                    GEOID %in% c(parea_intersecting_bgs$GEOID))

mapview::mapview(list( 
  extra_bgs
  ,metc_plus), col.regions = list("green", "blue"))


