# Part C of analysis.R, with (1) ratios normalised to 1, (2) 2002/03 baselines

# PART C - combine results  ----
# =============================================================================#
# setup to combine results
library(dplyr)
library(stringr)
library(tidyr)


## LGAs plus Greater Melbourne ----
## -----------------------------------------------------------------------------#
# get LGA names from output data files
LGA.output.files <- list.files("../Tables") %>% 
  str_subset(., "output_data_LGA_")

LGA.names <- LGA.output.files %>%
  str_replace("output_data_LGA_", "") %>%
  str_replace(".csv", "")

# read in the LGA files
for (i in 1:length(LGA.names)) {
  assign(LGA.names[i], read.csv(paste0("../Tables/", LGA.output.files[i])))
}

# read in the Greater Melbourne file
`Greater Melbourne` <- read.csv("../Tables/output_data_Greater Melbourne.csv")

# vector of all files
places <- c(LGA.names, "Greater Melbourne")

# dataframe to hold outputs
outputs.LGAs.Melb <- data.frame()

# add calculated fields to files
for (i in 1:length(places)) {
  
  # get the file 
  place <- get(places[i]) %>%
    # add location column
    mutate(location = places[i])
  
  # calculate ratios
  place <- place %>%
    rowwise() %>%
    mutate(train.tram.to.apt = sum(train.volume, tram.volume, na.rm = TRUE) / cum_hi_dens_dwel,
           train.tram.capadj.to.apt = sum(train.volume.capadj, tram.volume.capadj, na.rm = TRUE) / cum_hi_dens_dwel,
           train.tram.to.pop = sum(train.volume, tram.volume, na.rm = TRUE) / population,
           train.tram.capadj.to.pop = sum(train.volume.capadj, tram.volume.capadj, na.rm = TRUE) / population) %>%
    ungroup()
  
  # rebase the ratios so 2004 is 1
  train.tram.to.apt.base <- 
    place[place$fin_year_comp == 2004, "train.tram.to.apt"] %>% unlist()
  train.tram.capadj.to.apt.base <- 
    place[place$fin_year_comp == 2004, "train.tram.capadj.to.apt"] %>% unlist()
  train.tram.to.pop.base <- 
    place[place$fin_year_comp == 2004, "train.tram.to.pop"] %>% unlist()
  train.tram.capadj.to.pop.base <- 
    place[place$fin_year_comp == 2004, "train.tram.capadj.to.pop"] %>% unlist()
  
  place <- place %>%
    mutate(train.tram.to.apt = train.tram.to.apt / train.tram.to.apt.base,
           train.tram.capadj.to.apt = train.tram.capadj.to.apt / train.tram.capadj.to.apt.base,
           train.tram.to.pop = train.tram.to.pop / train.tram.to.pop.base,
           train.tram.capadj.to.pop = train.tram.capadj.to.pop / train.tram.capadj.to.pop.base)
  
  # replace the 2003 calculated figures with NA
  place[place$fin_year_comp == 2003, "train.tram.to.apt"] <- NA
  place[place$fin_year_comp == 2003, "train.tram.capadj.to.apt"] <- NA
  place[place$fin_year_comp == 2003, "train.tram.to.pop"] <- NA
  place[place$fin_year_comp == 2003, "train.tram.capadj.to.pop"] <- NA
  
  # add to the output dataframe
  outputs.LGAs.Melb <- rbind(outputs.LGAs.Melb,
                             place)
  
}

# convert to wide format
output.table.LGAs.Melb <- outputs.LGAs.Melb %>%  
  rename(year = fin_year_comp) %>%
  mutate(year = as.character(paste0("FY ", year-1, "/", str_sub(as.character(year), 3, 4)))) %>%
  pivot_wider(names_from = year,
              values_from = c(hi_dens_dwel, cum_hi_dens_dwel, train.volume, train.volume.capadj,
                              tram.volume, tram.volume.capadj, bus.volume, bus.volume.capadj,
                              population,
                              train.tram.to.apt, train.tram.capadj.to.apt,
                              train.tram.to.pop, train.tram.capadj.to.pop)) %>%
  # remove 'all NA' columns (FY 2002/03 where not starting apt and pop, bus before 2015)
  dplyr::select(where(~!all(is.na(.x)))) %>%
  # replace 'NaN' with 'NA
  replace(., . == 'NaN', NA)


# write result
write.csv(output.table.LGAs.Melb, 
          "../Tables/output_data_table_LGAs.csv", 
          row.names = FALSE)
