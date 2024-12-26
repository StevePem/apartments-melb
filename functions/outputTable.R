# function to assemble data outputs and calculate changes
# invoked from Part C of 'analysis.R'

outputFiles <- function(file.list) {
  # alphabetical order
  files <- sort(file.list)
  # numeric order
  numeric_parts <- as.numeric(str_extract(files, "\\d+"))
  files <- files[order(numeric_parts)]
  # routes (tram and bus) after others (train)
  files <- c(files[!(str_detect(files, "Route"))], 
             files[str_detect(files, "Route")])
  
  return(files)
}

outputTable <- function(files) {
  
  # dataframe to hold outputs
  outputs.df <- data.frame()
  
  # read in the files
  for (i in 1:length(files)) {
    
    # short place name
    place.name <- files[i] %>%
      str_replace("output_data_", "") %>%
      str_replace("LGA_", "") %>%
      str_replace("corridor_all_services_", "") %>%
      str_replace("corridor_own_services_", "") %>%
      str_replace(".csv", "")
    
    # read file
    assign(place.name, read.csv(paste0("../Tables/output data/", files[i])))
    
    # get the file 
    place <- get(place.name) %>%
      # add location column
      mutate(location = place.name) %>%
      # drop 2003 row
      filter(fin_year_comp != 2003) %>%
      # drop high_dens_dwel (annual apartments) column
      dplyr::select(-hi_dens_dwel)
    
    # calculate ratios ('train.tram' are train and tram only; 'services' includes
    # bus, though this is zero before 2015/16)
    place <- place %>%
      rowwise() %>%
      mutate(train.tram.to.apt = 
               sum(train.volume, tram.volume, na.rm = TRUE) / cum_hi_dens_dwel,
             train.tram.capadj.to.apt = 
               sum(train.volume.capadj, tram.volume.capadj, na.rm = TRUE) / cum_hi_dens_dwel,
             train.tram.to.pop = 
               sum(train.volume, tram.volume, na.rm = TRUE) / population,
             train.tram.capadj.to.pop = 
               sum(train.volume.capadj, tram.volume.capadj, na.rm = TRUE) / population,
             services.to.apt = 
               sum(train.volume, tram.volume, bus.volume, na.rm = TRUE) / cum_hi_dens_dwel,
             services.capadj.to.apt = 
               sum(train.volume.capadj, tram.volume.capadj, bus.volume.capadj, na.rm = TRUE) / cum_hi_dens_dwel,
             services.to.pop = 
               sum(train.volume, tram.volume, bus.volume, na.rm = TRUE) / population,
             services.capadj.to.pop = 
               sum(train.volume.capadj, tram.volume.capadj, bus.volume.capadj, na.rm = TRUE) / population) %>%
      ungroup()
    
    # add to the output dataframe
    outputs.df <- rbind(outputs.df,
                               place)
    
  }

  # convert to wide format and complete
  output.table <- outputs.df %>%  
    # years in FY format
    rename(year = fin_year_comp) %>%
    mutate(year = as.character(paste0("FY ", year-1, "/", str_sub(as.character(year), 3, 4)))) %>%
    # wide format
    pivot_wider(names_from = year,
                values_from = c(cum_hi_dens_dwel, 
                                train.volume, train.volume.capadj,
                                tram.volume, tram.volume.capadj, 
                                bus.volume, bus.volume.capadj,
                                population, pop_density, irsad,  ### CHECK - 'pop_density' was 'pop__density', not sure why
                                train.tram.to.apt, train.tram.capadj.to.apt,
                                train.tram.to.pop, train.tram.capadj.to.pop,
                                services.to.apt, services.capadj.to.apt,
                                services.to.pop, services.capadj.to.pop)) %>%
    # remove 'all NA' columns (FY 2002/03 where not starting apt and pop, bus before 2015, IRSAD non-census years)
    dplyr::select(where(~!all(is.na(.x)))) %>%
    # add 'change' columns for ratios (to 2004 for train.tram, to 2016 for services)
    mutate(train.tram.to.apt.change.2004.2022 = 
             (`train.tram.to.apt_FY 2021/22` - `train.tram.to.apt_FY 2003/04`)  / `train.tram.to.apt_FY 2003/04`,
           .after = `train.tram.to.apt_FY 2021/22`) %>%
    mutate(train.tram.capadj.to.apt.change.2004.2022 = 
             (`train.tram.capadj.to.apt_FY 2021/22` - `train.tram.capadj.to.apt_FY 2003/04`) / `train.tram.capadj.to.apt_FY 2003/04`,
           .after = `train.tram.capadj.to.apt_FY 2021/22`) %>%
    mutate(train.tram.to.pop.change.2004.2022 = 
             (`train.tram.to.pop_FY 2021/22` -  `train.tram.to.pop_FY 2003/04`) / `train.tram.to.pop_FY 2003/04`,
           .after = `train.tram.to.pop_FY 2021/22`) %>%
    mutate(train.tram.capadj.to.pop.change.2004.2022 = 
             (`train.tram.capadj.to.pop_FY 2021/22` - `train.tram.capadj.to.pop_FY 2003/04`) / `train.tram.capadj.to.pop_FY 2003/04`,
           .after = `train.tram.capadj.to.pop_FY 2021/22`) %>%
    mutate(services.to.apt.change.2016.2022 = 
             (`services.to.apt_FY 2021/22` - `services.to.apt_FY 2015/16`) / `services.to.apt_FY 2015/16`,
           .after = `services.to.apt_FY 2021/22`) %>%
    mutate(services.capadj.to.apt.change.2016.2022 = 
             (`services.capadj.to.apt_FY 2021/22` - `services.capadj.to.apt_FY 2015/16`) / `services.capadj.to.apt_FY 2015/16`,
           .after = `services.capadj.to.apt_FY 2021/22`) %>%
    mutate(services.to.pop.change.2016.2022 = 
             (`services.to.pop_FY 2021/22` - `services.to.pop_FY 2015/16`) / `services.to.pop_FY 2015/16`,
           .after = `services.to.pop_FY 2021/22`) %>%
    mutate(services.capadj.to.pop.change.2016.2022 = 
             (`services.capadj.to.pop_FY 2021/22` - `services.capadj.to.pop_FY 2015/16`) / `services.capadj.to.pop_FY 2015/16`,
           .after = `services.capadj.to.pop_FY 2021/22`) %>%
    # replace 'NaN' with 'NA
    replace(., . == 'NaN', NA)
  
  return(output.table)
  
}
  
  
