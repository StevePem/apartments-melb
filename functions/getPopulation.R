# functions for retrieving population data

# find intersection area for a given area and set of meshblocks 
# (used in getPopulation())
getSelectedMeshblockIntersection <- function(selected.area, MB) {
  return(
    selected.area %>%
      st_intersection(., MB) %>%
      mutate(isec.area = st_area(.),
             isec.pop = as.numeric((isec.area / orig.area) * Person)) %>%
      st_drop_geometry()
  )
}

# find populations for 2003 to 2022, based on census meshblock counts, plus
# SA2 population estimates for intervening years, scaled to align with the
# meshblock counts at the census years, plus SEIFA for census years
getPopulation <- function(selected.area, 
                          MB.2006, MB.2011, MB.2016, MB.2021, SA2.2021,
                          SEIFA.2006, SEIFA.2011, SEIFA.2016, SEIFA.2021) {
  
  # get meshblock intersections for selected area
  MB.isec.2006 <- getSelectedMeshblockIntersection(selected.area, MB.2006)
  MB.isec.2011 <- getSelectedMeshblockIntersection(selected.area, MB.2011)
  MB.isec.2016 <- getSelectedMeshblockIntersection(selected.area, MB.2016)
  MB.isec.2021 <- getSelectedMeshblockIntersection(selected.area, MB.2021)
  
  # get census year populations
  MB.pop.2006 <- MB.isec.2006 %>% .$isec.pop %>% sum(.)
  MB.pop.2011 <- MB.isec.2011 %>% .$isec.pop %>% sum(.)
  MB.pop.2016 <- MB.isec.2016 %>% .$isec.pop %>% sum(.)
  MB.pop.2021 <- MB.isec.2021 %>% .$isec.pop %>% sum(.)
  
  # get census year SEIFA deciles for census
  # SEIFA decile is the population-weighted mean of the deciles; see
  # https://www.abs.gov.au/statistics/people/people-and-communities/socio-economic-indexes-areas-seifa-australia/2021#data-downloads 
  # for ABS reference to population-weighted method
  MB.SEIFA.2006 <- MB.isec.2006 %>%
    summarise(mean.dec.wt = weighted.mean(Decile, Person, na.rm = TRUE))  %>%
    unlist() %>% unname()
  MB.SEIFA.2011 <- MB.isec.2011 %>%
    summarise(mean.dec.wt = weighted.mean(Decile, Person, na.rm = TRUE))  %>%
    unlist() %>% unname()
  MB.SEIFA.2016 <- MB.isec.2016 %>%
    summarise(mean.dec.wt = weighted.mean(Decile, Person, na.rm = TRUE))  %>%
    unlist() %>% unname()
  MB.SEIFA.2021 <- MB.isec.2021 %>%
    summarise(mean.dec.wt = weighted.mean(Decile, Person, na.rm = TRUE))  %>%
    unlist() %>% unname()
  
  # get annual SA2 pop estimates:
  # intersect SA2s with selected area
  SA2.isec <- selected.area %>%
    st_intersection(., SA2.2021) %>%
    mutate(isec.area = st_area(.)) %>%
    st_drop_geometry()
  
  # select years for which SA2 pop estimates are required
  years <- as.character(c(2003:2022))
  
  # for each year, calculate the SA2 pop estimate for the isec area
  for (i in 1:length(years)) {
    # name of variable, eg 'SA2.pop.2003'
    variable.name <- paste0("SA2.pop.", years[i])
    
    # table of orig area, isec area and pop for the relevant year; calculate pop
    SA2.isec.year.pop <- SA2.isec %>%
      dplyr::select(pop = any_of(paste0("pop_", years[i])),
                    orig.area,
                    isec.area) %>%
      mutate(isec.pop = as.numeric((isec.area / orig.area)) * as.numeric(pop)) %>%
      .$isec.pop %>%
      sum(.)
    
    # assign the output to the variable
    assign(variable.name, SA2.isec.year.pop)
    
  }
  
  # calculate population for each year, by scaling the SA2 populations so that
  # they align at census years with the census year meshblock populations,
  # using weighted average of the scaling factors for years between censuses
  scale.2006 <- MB.pop.2006 / SA2.pop.2006
  scale.2011 <- MB.pop.2011 / SA2.pop.2011
  scale.2016 <- MB.pop.2016 / SA2.pop.2016
  scale.2021 <- MB.pop.2021 / SA2.pop.2021
  
  
  pop.2003 <- SA2.pop.2003 * scale.2006
  pop.2004 <- SA2.pop.2004 * scale.2006
  pop.2005 <- SA2.pop.2005 * scale.2006
  pop.2006 <- SA2.pop.2006 * scale.2006  # same as 'MB.pop.2006'
  pop.2007 <- SA2.pop.2007 * ((scale.2006 * 4) + (scale.2011 * 1)) / 5
  pop.2008 <- SA2.pop.2008 * ((scale.2006 * 3) + (scale.2011 * 2)) / 5
  pop.2009 <- SA2.pop.2009 * ((scale.2006 * 2) + (scale.2011 * 3)) / 5
  pop.2010 <- SA2.pop.2010 * ((scale.2006 * 1) + (scale.2011 * 4)) / 5
  pop.2011 <- SA2.pop.2011 * scale.2011  # same as 'MB.pop.2011'
  pop.2012 <- SA2.pop.2012 * ((scale.2011 * 4) + (scale.2016 * 1)) / 5
  pop.2013 <- SA2.pop.2013 * ((scale.2011 * 3) + (scale.2016 * 2)) / 5
  pop.2014 <- SA2.pop.2014 * ((scale.2011 * 2) + (scale.2016 * 3)) / 5
  pop.2015 <- SA2.pop.2015 * ((scale.2011 * 1) + (scale.2016 * 4)) / 5
  pop.2016 <- SA2.pop.2016 * scale.2016  # same as 'MB.pop.2016'
  pop.2017 <- SA2.pop.2017 * ((scale.2016 * 4) + (scale.2021 * 1)) / 5
  pop.2018 <- SA2.pop.2018 * ((scale.2016 * 3) + (scale.2021 * 2)) / 5
  pop.2019 <- SA2.pop.2019 * ((scale.2016 * 2) + (scale.2021 * 3)) / 5
  pop.2020 <- SA2.pop.2020 * ((scale.2016 * 1) + (scale.2021 * 4)) / 5
  pop.2021 <- SA2.pop.2021 * scale.2021  # same as 'MB.pop.2021'
  pop.2022 <- SA2.pop.2022 * scale.2021
  
    
  # chk.table <- cbind(year = 2003:2022,
  #                    MB = c(NA, NA, NA,
  #                           MB.pop.2006, NA, NA, NA, NA,
  #                           MB.pop.2011, NA, NA, NA, NA,
  #                           MB.pop.2016, NA, NA, NA, NA,
  #                           MB.pop.2021, NA),
  #                    SA2 = c(SA2.pop.2003, SA2.pop.2004, SA2.pop.2005,
  #                            SA2.pop.2006, SA2.pop.2007, SA2.pop.2008,
  #                            SA2.pop.2009, SA2.pop.2010, SA2.pop.2011,
  #                            SA2.pop.2012, SA2.pop.2013, SA2.pop.2014,
  #                            SA2.pop.2015, SA2.pop.2016, SA2.pop.2017,
  #                            SA2.pop.2018, SA2.pop.2019, SA2.pop.2020,
  #                            SA2.pop.2021, SA2.pop.2022),
  #                    pop = c(pop.2003, pop.2004, pop.2005,
  #                            pop.2006, pop.2007, pop.2008,
  #                            pop.2009, pop.2010, pop.2011,
  #                            pop.2012, pop.2013, pop.2014,
  #                            pop.2015, pop.2016, pop.2017,
  #                            pop.2018, pop.2019, pop.2020,
  #                            pop.2021, pop.2022))

  
  # return outputs: first is population for years 2003-22; second is 
  # SEIFA for the census years, with other years 2003-22 padded by NAs
  return(list(c(pop.2003, pop.2004, pop.2005, pop.2006, pop.2007, pop.2008,
                pop.2009, pop.2010, pop.2011, pop.2012, pop.2013, pop.2014,
                pop.2015, pop.2016, pop.2017, pop.2018, pop.2019, pop.2020,
                pop.2021, pop.2022),
              c(NA, NA, NA, MB.SEIFA.2006, NA, NA,
                NA, NA, MB.SEIFA.2011, NA, NA, NA,
                NA, MB.SEIFA.2016, NA, NA, NA, NA,
                MB.SEIFA.2021, NA))
  )
  
}

# # compare sample SA2 2021 pop and its meshblock pops
# SA2 <- 209011204  # Watsonia
# SA2 <- 209011196  # Bundoora - East
# SA2.2021.pop <- SA2.2021 %>%
#   filter(SA2_CODE21 == SA2) %>%
#   .$pop_2021  # 9303 (Watsonia); 9925 (Bundoora - East)
# 
# meshblock.2021.pop <- MB.2021 %>%
#   filter(SA2_CODE21 == SA2) %>%
#  .$Person %>%
#   sum(.) #9121 (Watsonia); 9825 (Bundoora - East)
