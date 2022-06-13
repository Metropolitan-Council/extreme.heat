#####
## ACS data
#####

if (process_demos == TRUE) {
  #######
  ## Tidycensus
  #######
  # A portion of these data come from the tidycensus package, rather than the geospatial commons. I want more/different variables, and this seems like the easiest way to do this currently.
  
  # You will need an API key from Census:
  # - [Request an api key](https://api.census.gov/data/key_signup.html)
  # - Enter in the console: `usethis::edit_r_environ()`
  # - When the `.Renviron` file comes up in the editor, type: `CENSUS_KEY="KEY GOES HERE, INSIDE QUOTES"`
  # - Save and close the `.Renviron` file.
  # - Restart R.
  
  census_api_key(Sys.getenv("CENSUS_KEY"))
  
  v20 <- load_variables(2020, "acs5", cache = TRUE)
  View(v20)
  # v20 %>% filter(label == "Estimate!!Total:",
  #                geography == "block group",
  #                str_detect(concept, "HOUSE"))
  #  you may need to access the table shells: https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html
  
  acs_api <- get_acs(
    geography = "block group",
    variables = c(
      pop_total = "B01001_001",
      hh_total = "B11001_001",
      hh_1person = "B11016_010",
      hh_1personover65 = "B11007_003",
      income_percapita = "B19301_001"
    ),
    survey = "acs5",
    state = "MN",
    county = c("003", "019", "037", "053", "123", "139", "163"),
    year = 2020
  ) %>%
    rename(bg20 = GEOID) %>%
    select(-NAME) %>%
    pivot_wider(
      names_from = "variable",
      values_from = c("estimate", "moe"),
      names_sep = "."
    ) %>%
    mutate(
      estimate.hh_1person_percent = estimate.hh_1person / estimate.hh_total,
      moe.hh_1person_percent = moe_ratio(num = estimate.hh_1person, denom = estimate.hh_total, 
                                         moe_num = moe.hh_1person, moe_denom = moe.hh_total),
      
      estimate.hh_1personover65_percent = estimate.hh_1personover65 / estimate.hh_total,
      moe.hh_1personover65_percent = moe_ratio(num = estimate.hh_1personover65, denom = estimate.hh_total,
                                         moe_num = moe.hh_1personover65, moe_denom = moe.hh_total)
    ) %>%
    pivot_longer(names_to = "variable", values_to = "values",-bg20) %>%
    separate(variable,
             sep = "\\.",
             into = c("type", "variable")) %>%
    pivot_wider(names_from = type, values_from = values)# %>%
    # filter(variable %not_in% c("hh_total", "hh_1person", "hh_1personover65"))
  acs_api
  
  #I should remove these non-residential block groups
  acs_api %>% filter(variable == "pop_total",
                     estimate - moe <1) %>% view()
  #######
  # geospatial commons acs
  #######
  acs_geospatial <-
    read_csv(
      "/Volumes/shared/CommDev/Research/Research/Census Data/ACS_SF/2_OutputData/CSV/acs20205_bg.csv",
      col_select = c(
        "bg20" = "GEOG_UNIT",
        "POV185RATE",
        "MEDIANHHI",
        #"PCI", #income per capita
        "HOMEOWNPCT",
        "POPTOTAL",
        "LEP",
        #what about languages that cooling centers need to be advertised, or heat warnings made in
        "AGEUNDER18",
        "AGE65UP",
        "POPTOTAL"
      ),
      col_types = c("GEOG_UNIT" = "c", .default = "d")
    ) %>%
    janitor::clean_names() %>%
    mutate(
      lep_p = lep / poptotal,
      age18u_p = ageunder18 / poptotal,
      age65up_p = age65up / poptotal,
      age_sensitive = age65up_p + age18u_p
    ) %>%
    select(-lep,-poptotal,-ageunder18,-age65up) %>%
    pivot_longer(names_to = "variable", values_to = "estimate",-bg20)
  

  
  #########
  #### 2020 Census
  ########
  # The 2020 census is a population count vs survey, so it is a more accurate source for race demographics.
  
  temp <- tempfile()
  download.file(
    "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census2020population/xlsx_society_census2020population.zip",
    destfile = temp
  )
  census_race <-
    readxl::read_xlsx(unzip(temp, "Census2020PopulationBlockGroup.xlsx")) %>%
    janitor::clean_names() %>%
    transmute(
      bg20 = geog_unit,
      poptotal = poptotal,
      pblacknh = blacknh / poptotal,
      pasiannh = (asiannh + pacificnh) / poptotal,
      phisppop = hisppop / poptotal,
      pamindnh = amindnh / poptotal,
      pothmultnh = (multracenh + othernh) / poptotal,
      pbipoc = 1 - (whitenh / poptotal),
      
      poptot_mc = poptotal,
      hutot_mc = hhtotal
    )  %>%
    pivot_longer(names_to = "variable", values_to = "estimate",-bg20) %>%
    filter(variable %not_in% c("poptot_mc", "poptotal", "hutot_mc"))
  
  fs::file_delete("./Census2020PopulationBlockGroup.xlsx")


  #######  
  #### Synthesis
  #######
  var_names <- tribble(~variable, ~name,
    'age_sensitive', "Age, % in sensitive group (<18 or 65+)",
    'age18u_p', "Age, % under 18",
    "age65up_p", "Age, % 65+",
    "hh_p1person", "HH size, % 1-person",
    "hh_p1personover65", "HH size, % 1-person age 65+",
    "homeownpct", "Tenure, homeowner %",
    "income_percapita", "Income, per capita",
    "lep_p", "Language, % with limited english proficiency",
    "medianhhi", "Income, median household",
    "pamindnh", "Race, % American Indian",
    "pasiannh", "Race, % Asian",
    "pbipoc", "Race, % Person of Color",
    "pblacknh", "Race, % Black",
    "phisppop", "Race, % Hispanic",
    "pothmultnh", "Race, % Other or Multiracial",
    "pov185rate", "Income, % under 185% poverty rate"
  )
  
  demographics <- bind_rows(acs_api, acs_geospatial) %>%
    bind_rows(census_race) %>%
    filter(!is.na(estimate)) %>% #not sure why there are nas
    full_join(var_names)
  
  save(demographics, file = "./data/demographics.rda")
  
} else {
  load("./data/demographics.rda")
}
  
  
  # demos <- bind_rows(acs, census_race) %>%
  #   full_join(heat) %>%
  #   filter(!is.na(estimate),!is.na(heat2016)) #not sure why there are so many nas as this point