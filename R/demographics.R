#####
## ACS data
#####

if (process_demos == TRUE) {
  #######
  ## Tidycensus
  #######
  # A portion of these data come from the tidycensus package, rather than the geospatial commons. I want more/different variables, and this seems like the easiest way to do this currently.
  
  library(tidycensus)
  library(tidyverse)
  `%not_in%` <- Negate(`%in%`)
  
  # You will need an API key from Census:
  # - [Request an api key](https://api.census.gov/data/key_signup.html)
  # - Enter in the console: `usethis::edit_r_environ()`
  # - When the `.Renviron` file comes up in the editor, type: `CENSUS_KEY="KEY GOES HERE, INSIDE QUOTES"`
  # - Save and close the `.Renviron` file.
  # - Restart R.
  
  census_api_key(Sys.getenv("CENSUS_KEY"))
  
  v20 <- load_variables(2020, "acs5", cache = TRUE)
  View(v20 %>% filter(geography == "block group"))
  # v20 %>% filter(label == "Estimate!!Total:",
  #                geography == "block group",
  #                str_detect(concept, "HOUSE"))
  #  you may need to access the table shells: https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html
  #  census reporter topics are also very useful! https://censusreporter.org/topics/

  fxn_sum_acs <- function(.vars, .name){
    get_acs(
      geography = "block group",
      variables = .vars,
      survey = "acs5",
      state = "MN",
      county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"),
      year = 2020) %>%
      group_by(GEOID) %>%
      summarise(sumest = sum(estimate),
                summoe = moe_sum(moe, estimate)) %>%
      mutate(variable = .name) %>%
      rename(estimate = sumest,
             moe = summoe)
  }
  # # I'd like to make this work, but am having a hard time :/ 
  # fxn_calc_percent <- function(x, .num, .denom){
  #   # .num_2 <- paste0("estimate.", as_label(enquo(.num)))
  #   x %>%
  #     mutate("{{.num}}_percent" := !!enquo(.num) / !!enquo(.denom))
  #   # paste0("estimate.", (.num), "_percent") = paste0("estimate.", (.num)) / paste0("estimate.", (.denom))
  #   # paste0("moe.", !!enquo(.num), "_percent") = moe_ratio(num = paste0("estimate.", !!enquo(.num)), denom = paste0("estimate.", !!.enquo(.denom)),
  #   #                                    moe_num = paste0("moe.", !!enquo(.num)), moe_denom = paste0("moe.", !!.enquo(.denom)))
  # }
  # 
  # acsapi %>% head() %>% select(bg20, estimate.age_sensitive, estimate.pop_total) %>%
  #   fxn_calc_percent(.num = age_sensitive, .denom = estimate.pop_total)

  
   acs_age <- fxn_sum_acs(.vars = c(paste0("B01001_00", c(3:6)), #under18 m
                                     paste0("B01001_0", c(27:30)), #under18 f
                                     paste0("B01001_0", c(20:25, 44:49)) #over 65m, f
                                     ),
                           .name = "age_sensitive") %>%
     bind_rows(fxn_sum_acs(.vars = c(paste0("B01001_00", c(3:6)), #under18 m
                                     paste0("B01001_0", c(27:30))), #under18 f
                           .name = "age_under18")) %>%
     bind_rows(fxn_sum_acs(.vars = c(paste0("B01001_0", c(20:25, 44:49))), #over 65m, f
                                     .name = "age_65up"))
   
   acs_income <- fxn_sum_acs(.vars = c(paste0("C17002_00", c(2:6))),
                  .name = "income_below185pov")
  
  acsapi_sums <- bind_rows(acs_age, acs_income)
  
  acsapi_persons <- get_acs(
    geography = "block group",
    variables = c(
      pop_total = "B01001_001",
      
      hh_total = "B11016_001",
      hh_1person = "B11016_010",
      
      hhage_total = "B11007_001",
      hhage_1personover65 = "B11007_003",
      
      income_percapita = "B19301_001",
      income_median = "B19013_001",
      income_povstatus = "C17002_001",
      
      lang_total = "C16002_001",
      lang_eng = "C16002_002",
      
      tenure_total = "B25003_001",
      tenure_renter = "B25003_003",
      tenure_contractrent = "B25058_001", #Gross rent is similar to selected monthly owner costs. It is the sum of contract rent and the average cost of the utilities (electricity, gas, and water and sewer) and fuel (oil, coal, kerosene, wood, etc).
      tenure_grossrent = "B25064_001",
      
      comm_total = "B28002_001",
      comm_nointernet = "B28002_013",
      comm_cellonly = "B28002_006",
      comm_yesinternet = "B28002_002"
    ),
    survey = "acs5",
    state = "MN",
    county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"),#c("003", "019", "037", "053", "123", "139", "163"),
    year = 2020
  ) %>%
    bind_rows(acsapi_sums) %>%
    
    rename(bg20 = GEOID) %>%
    select(-NAME) 
  
  acsapi_percents <- acsapi_persons %>%
    pivot_wider(
      names_from = "variable",
      values_from = c("estimate", "moe"),
      names_sep = "."
    ) %>%
      mutate(
      
      estimate.tenure_utilitycost = (estimate.tenure_grossrent - estimate.tenure_contractrent) / estimate.tenure_contractrent,
      moe.tenure_utilitycost = moe.tenure_grossrent,
      
      estimate.hh_1person_percent = estimate.hh_1person / estimate.hh_total,
      moe.hh_1person_percent = moe_ratio(num = estimate.hh_1person, denom = estimate.hh_total, 
                                         moe_num = moe.hh_1person, moe_denom = moe.hh_total),
      
      estimate.hhage_1personover65_percent = estimate.hhage_1personover65 / estimate.hhage_total,
      moe.hhage_1personover65_percent = moe_ratio(num = estimate.hhage_1personover65, denom = estimate.hhage_total,
                                         moe_num = moe.hhage_1personover65, moe_denom = moe.hhage_total),
      
      estimate.lang_eng_percent = (estimate.lang_eng) / estimate.lang_total,
      moe.lang_eng_percent = moe_ratio(num = (estimate.lang_eng), denom = estimate.lang_total,
                                               moe_num = moe.lang_eng, moe_denom = moe.lang_total),
      
      estimate.tenure_owner_percent = (estimate.tenure_total - estimate.tenure_renter) / estimate.tenure_total,
      moe.tenure_owner_percent = moe_ratio(num = estimate.tenure_renter, denom = estimate.tenure_total,
                                            moe_num = moe.tenure_renter, moe_denom = moe.tenure_total),

            estimate.tenure_renter_percent = estimate.tenure_renter / estimate.tenure_total,
      moe.tenure_renter_percent = moe_ratio(num = estimate.tenure_renter, denom = estimate.tenure_total,
                                            moe_num = moe.tenure_renter, moe_denom = moe.tenure_total),
      
      estimate.age_sensitive_percent = estimate.age_sensitive / estimate.pop_total,
      moe.age_sensitive_percent = moe_ratio(num = estimate.age_sensitive, denom = estimate.pop_total,
                                            moe_num = moe.age_sensitive, moe_denom = moe.pop_total),

      estimate.age_under18_percent = estimate.age_under18 / estimate.pop_total,
      moe.age_under18_percent = moe_ratio(num = estimate.age_under18, denom = estimate.pop_total,
                                            moe_num = moe.age_under18, moe_denom = moe.pop_total),

      estimate.age_65up_percent = estimate.age_65up / estimate.pop_total,
      moe.age_65up_percent = moe_ratio(num = estimate.age_65up, denom = estimate.pop_total,
                                          moe_num = moe.age_65up, moe_denom = moe.pop_total),
      
      estimate.comm_nointernet_percent = estimate.comm_nointernet / estimate.comm_total,
      moe.comm_nointernet_percent = moe_ratio(num = estimate.comm_nointernet, denom = estimate.comm_total,
                                            moe_num = moe.comm_nointernet, moe_denom = moe.comm_total),
      
      estimate.comm_yesinternet_percent = estimate.comm_yesinternet / estimate.comm_total,
      moe.comm_yesinternet_percent = moe_ratio(num = estimate.comm_yesinternet, denom = estimate.comm_total,
                                            moe_num = moe.comm_yesinternet, moe_denom = moe.comm_total),

            estimate.comm_cellonly_percent = estimate.comm_cellonly / estimate.comm_total,
      moe.comm_cellonly_percent = moe_ratio(num = estimate.comm_cellonly, denom = estimate.comm_total,
                                            moe_num = moe.comm_cellonly, moe_denom = moe.comm_total),
      
      estimate.income_below185pov_percent = estimate.income_below185pov / estimate.income_povstatus,
      moe.income_below185pov_percent = moe_ratio(num = estimate.income_below185pov, denom = estimate.income_povstatus,
                                       moe_num = moe.income_below185pov, moe_denom = moe.income_povstatus)
    ) %>% 
    pivot_longer(names_to = "variable", values_to = "values",-bg20) %>%
    separate(variable,
             sep = "\\.",
             into = c("type", "variable")) %>%
    pivot_wider(names_from = type, values_from = values) %>%
    # mutate(flag = if_else(estimate - moe <= 0, 1, NA_real_)) %>% #remove instances where moe overlaps with zero; ACTUALLY, what to do when the percent IS zero?!?! (percent english, or something)
    # filter(is.na(flag),) %>% select(-flag) %>%
    filter(str_detect(variable, "percent|income|tenure_utilitycost")) %>%
    filter(variable %not_in% c("income_povstatus", "income_below185pov",
                               "tenure_grossrent", "tenure_contractrent"))
    # filter(!str_detect(variable, "_total"), #remove variables used only to calculate percentages
    #        variable %not_in% c("age_sensitive",
    #                            "income_pov_status", "income_below185pov",
    #                            "hhage_1personover65", "hh_1person"))  

  # view(acsapi)
    # filter(variable %not_in% c("hh_total", "hh_1person", "hh_1personover65"))

  # #I should remove these non-residential block groups
  # acsapi %>% filter(variable == "pop_total",
  #                    estimate - moe <1) %>% view()
  
  
  # #######
  # # geospatial commons acs
  # # I have fully transitioned away! yay!
  # #######
  # acs_geospatial <-
  #   read_csv(
  #     "/Volumes/shared/CommDev/Research/Research/Census Data/ACS_SF/2_OutputData/CSV/acs20205_bg.csv",
  #     col_select = c(
  #       "bg20" = "GEOG_UNIT",
  #       "POV185RATE",
  #       # "MEDIANHHI",
  #       #"PCI", #income per capita
  #       # "HOMEOWNPCT",
  #       "POPTOTAL",
  #       # "LEP",
  #       #what about languages that cooling centers need to be advertised, or heat warnings made in
  #       # "AGEUNDER18",
  #       # "AGE65UP",
  #       "POPTOTAL"
  #     ),
  #     col_types = c("GEOG_UNIT" = "c", .default = "d")
  #   ) %>%
  #   janitor::clean_names() %>%
  #   mutate(
  #     lep_p = lep / poptotal,
  #     age18u_p = ageunder18 / poptotal,
  #     age65up_p = age65up / poptotal,
  #     age_sensitive = age65up_p + age18u_p
  #   ) %>%
  #   select(-lep,-poptotal,-ageunder18,-age65up) %>%
  #   pivot_longer(names_to = "variable", values_to = "estimate",-bg20)
  # 

  
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
  
  census_persons <-
    readxl::read_xlsx(unzip(temp, "Census2020PopulationBlockGroup.xlsx")) %>%
    janitor::clean_names() %>%
    transmute(
      bg20 = geog_unit,
      pblacknh = blacknh,
      pasiannh = (asiannh + pacificnh) ,
      phisppop = hisppop,
      pamindnh = amindnh ,
      pothmultnh = (multracenh + othernh),
      pbipoc = poptotal - (whitenh)
      
    )  %>%
    pivot_longer(names_to = "variable", values_to = "estimate",-bg20) 

  fs::file_delete("./Census2020PopulationBlockGroup.xlsx")


  #######  
  #### Synthesis
  #######
  var_names <- tribble(~variable, ~name,
                       'age_under18_percent', "Age; % under 18",
                       'age_65up_percent', "Age; % 65 and up",
                       'age_sensitive_percent', "Age; % in sensitive group (<18 or 65+)",
    "hh_1person_percent", "Households; % 1-person households",
    "hhage_1personover65_percent", "Households; % 1-person households age 65+",
    "tenure_renter_percent", "Tenure; renter %",
    "tenure_owner_percent", "Tenure; owner %",
    "tenure_utilitycost", "Tenure; median renter utility costs as a percent of contract rent",
    "income_percapita", "Income; per capita",
    "income_below185pov_percent", "Income; % under 185% poverty rate",
    "comm_cellonly_percent", "Communications; % accessing internet via cellphone only",
    "comm_nointernet_percent", "Communications; % without internet at home",
    "comm_yesinternet_percent", "Communications; % with internet",
    "lang_eng_percent", "Communications; % speaking English at home",
    "income_median", "Income; median household",
    "pamindnh", "Race; % American Indian",
    "pasiannh", "Race; % Asian",
    "pbipoc", "Race; % Person of Color",
    "pblacknh", "Race; % Black",
    "phisppop", "Race; % Hispanic",
    "pothmultnh", "Race; % Other or Multiracial"
  )
  
  demographics <- (acsapi_percents %>% add_column(data = "percents")) %>%
    bind_rows(acsapi_persons %>% add_column(data = "persons"))%>% #bind_rows(acsapi, acs_geospatial) %>%
    bind_rows(census_race %>% add_column(data = "percents")) %>%
    bind_rows(census_persons %>% add_column(data = "persons")) %>%
    filter(!is.na(estimate)) %>% #not sure why there are nas
    full_join(var_names)

  save(demographics, file = "./data/demographics.rda")
  
} else {
  load("./data/demographics.rda")
}
  
  
  # demos <- bind_rows(acs, census_race) %>%
  #   full_join(heat) %>%
  #   filter(!is.na(estimate),!is.na(heat2016)) #not sure why there are so many nas as this point