##### Extreme heat, 2016 data

if (process_heat == TRUE){
  
  heat <-
  readxl::read_xlsx("/Users/escheh/Documents/GitHub/planting.shade/data-raw/CLIMATE_BG20.xlsx") %>%
  janitor::clean_names() %>%
  transmute(bg20 = bg20,
            heat2016 = avg_temp)
  
  save(heat, file = "./data/heat.rda")
  
} else {
  load("./data/heat.rda")
}