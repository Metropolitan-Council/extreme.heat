##### Tree canopy, 2021 data


##### Impervious surfaces
# from UMN 1m2 land cover
# processing comes from GEE: https://code.earthengine.google.com/?scriptPath=users%2Fehe%2FMetCouncil%3ASurfaceWithPurpose

# impervious surfaces
# ndvi
# tree canopy percent

## 
if (process_environment == TRUE){
  
  load("/Users/escheh/Documents/GitHub/planting.shade/data/mn_bgs.rda")
  growingshade <- bg_growingshade_main %>%
    filter(variable %in% c("canopy_percent", "ndvi_land")) %>%
    rename(bg20 = bg_string,
           value = raw_value) %>%
    select(bg20, variable, name, value)
  

  environment <- growingshade 
    
  
  save(environment, file = "./data/environment.rda")
  
} else {
  load("./data/environment.rda")
}