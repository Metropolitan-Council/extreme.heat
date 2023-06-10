##### Extreme heat, 2016 data

if (process_heat == TRUE){
  
  # heat <-
  # readxl::read_xlsx("/Users/escheh/Documents/GitHub/planting.shade/data-raw/CLIMATE_BG20.xlsx") %>%
  # janitor::clean_names() %>%
  # transmute(bg20 = bg20,
  #           heat2016 = avg_temp)
 
  heat <- read_csv("/Users/escheh/Downloads/ExtremeHeat2022_blockgroups2020.csv",
                              col_select = c(GEOID, mean_LST),
                              col_types = c("GEOID" = "c")) %>%
    mutate(mean_LST = stringr::str_remove_all(mean_LST, "\\{LST=|\\}|null"),
           mean_LST = as.numeric(mean_LST)) %>%
    rename(bg20 = GEOID,
           heat2022 = mean_LST) %>%
    filter(!is.na(heat2022))
  heat
  # levels(as.factor(heat$mean_LST))
  save(heat, file = "./data/heat.rda")
  
} else {
  load("./data/heat.rda")
}

