# [functions to get train volumes]


# function to find the total line.vol for a given month, year and group of routes
getLineVol <- function(line.vol, year, month, routes) {
  # find rows which match Year and Month, then sum their sched.vol
  return(line.vol[line.vol$Year == year &
                    line.vol$Month == month &
                    line.vol$Route %in% unlist(strsplit(routes, ", ")), ] %>%
           .$sched.vol %>% 
           sum())
}


# function to find train capacity for a given station and 'month', where
# 'month' is in the format "Jul_2003"
get.train.cap <- function(train.captable, station, routes, month) {
 
   # find the row in train.captable containing the relevant month
  selected.row <- case_when(
    # financial year rows
    to_date(month) >= to_date("Jul_2003") & to_date(month) <= to_date("Jun_2004") ~ "FY_2004",
    to_date(month) >= to_date("Jul_2004") & to_date(month) <= to_date("Jun_2005") ~ "FY_2005",
    to_date(month) >= to_date("Jul_2005") & to_date(month) <= to_date("Jun_2006") ~ "FY_2006",
    to_date(month) >= to_date("Jul_2006") & to_date(month) <= to_date("Jun_2007") ~ "FY_2007",
    to_date(month) >= to_date("Jul_2007") & to_date(month) <= to_date("Jun_2008") ~ "FY_2008",
    to_date(month) >= to_date("Jul_2008") & to_date(month) <= to_date("Jun_2009") ~ "FY_2009",
    to_date(month) >= to_date("Jul_2009") & to_date(month) <= to_date("Jun_2010") ~ "FY_2010",
    to_date(month) >= to_date("Jul_2010") & to_date(month) <= to_date("Jun_2011") ~ "FY_2011",
    to_date(month) >= to_date("Jul_2011") & to_date(month) <= to_date("Jun_2012") ~ "FY_2012",
    to_date(month) >= to_date("Jul_2012") & to_date(month) <= to_date("Jun_2013") ~ "FY_2013",
    to_date(month) >= to_date("Jul_2013") & to_date(month) <= to_date("Jun_2014") ~ "FY_2014",
    to_date(month) >= to_date("Jul_2014") & to_date(month) <= to_date("Jun_2015") ~ "FY_2015",
    to_date(month) >= to_date("Jul_2015") & to_date(month) <= to_date("Jun_2016") ~ "FY_2016",
    to_date(month) >= to_date("Jul_2016") & to_date(month) <= to_date("Jun_2017") ~ "FY_2017",
    to_date(month) >= to_date("Jul_2017") & to_date(month) <= to_date("Jun_2018") ~ "FY_2018",
    to_date(month) >= to_date("Jul_2018") & to_date(month) <= to_date("Jun_2019") ~ "FY_2019",
    to_date(month) >= to_date("Jul_2019") & to_date(month) <= to_date("Jun_2020") ~ "FY_2020",
    # month rows - convert from "Jul_2020" format to "2020-07" format
    TRUE ~ strftime(as.Date(paste0(month, "_01"), format = "%B_%Y_%d"), format = "%Y-%m")
  )
  
  # find the column in train.captable for the relevant route/station combination
  selected.column <- case_when(
    routes == "Alamein"
      ~ "Alamein",
    routes == "Alamein, Belgrave, Craigieburn, Cranbourne, Frankston, Glen Waverley, Hurstbridge, Lilydale, Mernda, Pakenham, Sandringham, Sunbury, Upfield, Werribee, Williamstown"
      ~ "Stn.City",
    routes == "Alamein, Belgrave, Cranbourne, Frankston, Glen Waverley, Lilydale, Pakenham, Sandringham"
      ~ "Stn.Richmond",                                                                            
    routes == "Alamein, Belgrave, Glen Waverley, Lilydale" & station == "East Richmond"
      ~ "Glen.Waverley",                                                                                                                           
    routes == "Alamein, Belgrave, Glen Waverley, Lilydale" & station == "Burnley"
      ~ "Stn.Burnley",                                                                                                                           
    routes == "Alamein, Belgrave, Lilydale"
      ~ "Stn.Hawthorn_Camberwell",                                                                                                                                          
    routes == "Belgrave" 
      ~ "Belgrave.Lilydale",                                                                                                                                                            
    routes == "Belgrave, Lilydale"
      ~ "Belgrave.Lilydale",                                                                                                                                                   
    routes == "Craigieburn"
      ~ "Craigieburn",                                                                                                                                                          
    routes == "Craigieburn, Sunbury, Upfield, Werribee, Williamstown"
      ~ "Stn.North.Melbourne",
    routes == "Cranbourne"
      ~ "Pakenham.Cranbourne",
    routes == "Cranbourne, Frankston, Pakenham" & station %in% c("Hawksburn", "Toorak", "Armadale")
      ~ "Stn.Hawksburn_Armadale",                                                                                                                                      
    routes == "Cranbourne, Frankston, Pakenham" & station %in% c("Malvern", "Caulfield")
      ~ "Stn.Malvern_Caulfield",                                                                                                                                      
    routes == "Cranbourne, Frankston, Pakenham, Sandringham"
      ~ "Stn.South.Yarra",                                                                                                                         
    routes == "Cranbourne, Pakenham"
      ~ "Pakenham.Cranbourne",                                                                                                                                                 
    routes == "Frankston"
      ~ "Frankston",                                                                                                                                                            
    routes == "Frankston, Stony Point"
      ~ "Frankston",                                                                                                                                               
    routes == "Glen Waverley" 
      ~ "Glen.Waverley",                                                                                                                                                       
    routes == "Hurstbridge" 
      ~ "Hurstbridge",                                                                                                                                                         
    routes == "Hurstbridge, Mernda" 
      ~ "Stn.Jolimont_Clifton.Hill",                                                                                                                                                 
    routes == "Lilydale"     
      ~ "Belgrave.Lilydale",                                                                                                                                                        
    routes == "Mernda"    
      ~ "Epping",
    routes == "Pakenham"
      ~ "Pakenham.Cranbourne",
    routes == "Sandringham"
      ~ "Sandringham",                                                                                                                                                          
    routes == "Stony Point" 
      ~ "Stony.Point",                                                                                                                                                         
    routes == "Sunbury"  
      ~ "Sunbury",                                                                                                                                                            
    routes == "Sunbury, Werribee, Williamstown" & station == "South Kensington"
      ~ "Stn.South.Kensington",                                                                                                                                     
    routes == "Sunbury, Werribee, Williamstown" & station == "Footscray"
    ~ "Stn.Footscray",                                                                                                                                     
    routes == "Upfield" 
      ~ "Upfield",                                                                                                                                                             
    routes == "Werribee" 
      ~ "Werribee",                                                                                                                                                            
    routes == "Werribee, Williamstown"
      ~ "Stn.Seddon_Newport",                                                                                                                                               
    routes == "Williamstown" 
      ~ "Williamstown"  
  )
  
  # return the capacity factor from the selected row and column
  return(train.captable[train.captable == selected.row, selected.column])
  
}


# collapse train line volumes for lines that feed into each other, keeping
# the highest volume at each junction point

collapseVolumes <- function(vol.month, vol.col) {
  
  # rename the volume column with standardised name, and remove any other vol columns
  vol.month <- vol.month %>%
    rename(vol = all_of(vol.col)) %>%
    dplyr::select(Routes, Month, vol)
  
  # get the current month
  month <- vol.month$Month %>% unique()
  
  # add any missing line groups for the month with zero
  lines <- c("Alamein",                                                                                                                                                             
             "Alamein, Belgrave, Craigieburn, Cranbourne, Frankston, Glen Waverley, Hurstbridge, Lilydale, Mernda, Pakenham, Sandringham, Sunbury, Upfield, Werribee, Williamstown",
             "Alamein, Belgrave, Cranbourne, Frankston, Glen Waverley, Lilydale, Pakenham, Sandringham",                                                                            
             "Alamein, Belgrave, Glen Waverley, Lilydale",                                                                                                                          
             "Alamein, Belgrave, Lilydale",                                                                                                                                         
             "Belgrave",                                                                                                                                                            
             "Belgrave, Lilydale",                                                                                                                                                  
             "Craigieburn",                                                                                                                                                         
             "Craigieburn, Sunbury, Upfield, Werribee, Williamstown",                                                                                                               
             "Cranbourne",                                                                                                                                                          
             "Cranbourne, Frankston, Pakenham",                                                                                                                                     
             "Cranbourne, Frankston, Pakenham, Sandringham",                                                                                                                        
             "Cranbourne, Pakenham",                                                                                                                                                
             "Frankston",                                                                                                                                                           
             "Frankston, Stony Point",                                                                                                                                              
             "Glen Waverley",                                                                                                                                                       
             "Hurstbridge",                                                                                                                                                         
             "Hurstbridge, Mernda",                                                                                                                                                 
             "Lilydale",                                                                                                                                                            
             "Mernda",                                                                                                                                                              
             "Pakenham",                                                                                                                                                            
             "Sandringham",                                                                                                                                                         
             "Stony Point",                                                                                                                                                         
             "Sunbury",                                                                                                                                                             
             "Sunbury, Werribee, Williamstown",                                                                                                                                     
             "Upfield",                                                                                                                                                             
             "Werribee",                                                                                                                                                            
             "Werribee, Williamstown",                                                                                                                                              
             "Williamstown")
  
  for (k in 1:length(lines)) {
    if (!lines[k] %in% vol.month$Routes) {
      vol.month <- rbind(vol.month, 
                         c(Routes = lines[k],
                           Month = month,
                           vol = 0)) 
    }
  }
  
  # function to get the volume for a given line
  get.vol <- function(line) {
    return(as.numeric(vol.month[vol.month$Routes == line, "vol"][[1]]))
  }
  
  # Collapse to get highest combined volumes
  # Burnley group
  # collapse at Ringwood
  vol.Ringwood <- max(get.vol("Belgrave") + get.vol("Lilydale"),
                      get.vol("Belgrave, Lilydale"))
  
  # collapse at Camberwell
  vol.Camberwell <- max(vol.Ringwood + get.vol("Alamein"),
                        get.vol("Alamein, Belgrave, Lilydale"))
  
  # collapse at Burnley
  vol.Burnley <- max(vol.Camberwell + get.vol("Glen Waverley"),
                     get.vol("Alamein, Belgrave, Glen Waverley, Lilydale"))
  
  
  # South Yarra group
  # collapse at Dandenong
  vol.Dandenong <- max(get.vol("Cranbourne") + get.vol("Pakenham"),
                       get.vol("Cranbourne, Pakenham"))
  
  # collapse at Frankston 
  vol.Frankston <- max(get.vol("Frankston") + get.vol("Stony Point"),
                       get.vol("Frankston, Stony Point"))
  
  # collapse at Caulfield
  vol.Caulfield <- max(vol.Dandenong + vol.Frankston,
                       get.vol("Cranbourne, Frankston, Pakenham"))
  
  
  # collapse at South Yarra 
  vol.SouthYarra <- max(vol.Caulfield + get.vol("Sandringham"),
                        get.vol("Cranbourne, Frankston, Pakenham, Sandringham"))
  
  
  # Combined Burnley/South Yarra groups
  # collapse at Richmond
  vol.Richmond <- max(vol.Burnley + vol.SouthYarra,
                      get.vol("Alamein, Belgrave, Cranbourne, Frankston, Glen Waverley, Lilydale, Pakenham, Sandringham"))
  
  
  # Clifton Hill group
  # collapse at Clifton Hill
  vol.CliftonHill <- max(get.vol("Hurstbridge") + get.vol("Mernda"),
                         get.vol("Hurstbridge, Mernda"))
  
  
  # North Melbourne group
  # collapse at Newport
  vol.Newport <- max(get.vol("Werribee") + get.vol("Williamstown"),
                     get.vol("Werribee, Williamstown"))
  
  # collapse at Footscray
  vol.Footscray <- max(vol.Newport + get.vol("Sunbury"),
                       get.vol("Sunbury, Werribee, Williamstown"))
  
  # collapse at North Melbourne
  vol.NorthMelbourne <- max(vol.Footscray + get.vol("Craigieburn") + get.vol("Upfield"),
                            get.vol("Craigieburn, Sunbury, Upfield, Werribee, Williamstown"))
  
  
  # Combine totals in city
  vol.City <- max(vol.Richmond + vol.CliftonHill + vol.NorthMelbourne,
                  get.vol("Alamein, Belgrave, Craigieburn, Cranbourne, Frankston, Glen Waverley, Hurstbridge, Lilydale, Mernda, Pakenham, Sandringham, Sunbury, Upfield, Werribee, Williamstown"))
  
  
  # return the combined total
  return(vol.City)
  
}

