# extract from former version of dashboard.Rmd that did the data processing 
# within the app (rather than pre-processing)


```{r data}
LGAs <- read.csv("../Tables/output_data_table_LGAs.csv") %>%
  # fix Greater Melbourne name  # TO DO: appears as '/Greater Melbourne'; fix in analysis.R if possible
  mutate(location = ifelse(grepl("Greater Melbourne", location), 
                           "Greater Melbourne", location))

corridors <- read.csv("../Tables/output_data_table_corridor_own_services_with_apts.csv") %>%
  # remove "with_apts_'
  mutate(location = gsub("with_apts_", "", location))

LGA.names <- LGAs$location[!grepl("Greater Melbourne", LGAs$location)]

# corridors: train have no route no; tram route no under 200; bus route no 200+
corridors.train <- corridors$location[!grepl("Route", corridors$location)]
corridors.tram <-corridors$location[grepl("Route", corridors$location) &
                                      as.numeric(gsub("\\D", "", corridors$location)) < 200]
corridors.bus <- corridors$location[grepl("Route", corridors$location) &
                                      as.numeric(gsub("\\D", "", corridors$location)) >= 200]

# data into long form
all.data <- bind_rows(LGAs, corridors) %>%
  # select only required columns
  dplyr::select(location,
                contains("cum_hi_dens_dwel"),
                contains("population"),
                contains("train.volume"),
                contains("tram.volume"),
                contains("bus.volume")) %>%
  
  # index so starting value is 100 = NO THIS NEEDS TO BE DONE LATER
  mutate(orig.dwel = cum_hi_dens_dwel_FY.2003.04,
         orig.pop = population_FY.2003.04,
         orig.train = train.volume_FY.2003.04,
         orig.train.capadj = train.volume.capadj_FY.2003.04,
         orig.tram = tram.volume_FY.2003.04,
         orig.tram.capadj = tram.volume.capadj_FY.2003.04,
         orig.bus = bus.volume_FY.2015.16,
         orig.bus.capadj = bus.volume.capadj_FY.2015.16) %>%
  mutate_at(vars(contains("cum_hi_dens_dwel")), ~ . / orig.dwel * 100) %>%
  mutate_at(vars(contains("population")), ~ . / orig.pop * 100) %>%
  mutate_at(vars(contains("train.volume_FY")), ~ . / orig.train * 100) %>%
  mutate_at(vars(contains("train.volume.capadj_FY")), ~ . / orig.train.capadj * 100) %>%
  mutate_at(vars(contains("tram.volume_FY")), ~ . / orig.tram * 100) %>%
  mutate_at(vars(contains("tram.volume.capadj_FY")), ~ . / orig.tram.capadj * 100) %>%
  mutate_at(vars(contains("bus.volume_FY")), ~ . / orig.bus * 100) %>%
  mutate_at(vars(contains("bus.volume.capadj_FY")), ~ . / orig.bus.capadj * 100) %>%
  dplyr::select(-contains("orig")) %>%
  
  # convert to long form
  pivot_longer(!location, names_to = c("variable", "year"), 
               names_sep = "_FY.", values_to = "value") %>%
  
  # tidy up variable names
  mutate(variable = case_when(
    variable == "cum_hi_dens_dwel"    ~ "apartments",
    variable == "train.volume"        ~ "train",
    variable == "train.volume.capadj" ~ "train capadj",
    variable == "tram.volume"         ~ "tram",
    variable == "tram.volume.capadj"  ~ "tram capadj",
    variable == "bus.volume"          ~ "bus",
    variable == "bus.volume.capadj"   ~ "bus capadj",
    TRUE                              ~ variable
  ))


```