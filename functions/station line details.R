# function to return a table that shows, for each metro train station, the 
# line(s) on which it is situated

stationLineDetails <- function(stations) {
  return(
    names(stations)[!names(stations) %in% c("Month", "Year", "Grand Total")] %>%
      as.data.frame() %>%
      setNames(., c("Station")) %>%
      mutate(Routes = case_when(
        # single lines
        Station %in% c("Alamein", "Ashburton", "Burwood", "Hartwell", 
                       "Riversdale", "Willison")                      
        ~ "Alamein",
        
        Station %in% c("Bayswater", "Belgrave", "Boronia", "Ferntree Gully",
                       "Heathmont", "Tecoma", "Upper Ferntree Gully", "Upwey")
        ~ "Belgrave",
        
        Station %in% c("Ascot Vale","Broadmeadows", "Coolaroo", "Craigieburn",
                       "Essendon", "Glenbervie", "Glenroy", "Jacana",
                       "Kensington", "Moonee Ponds", "Newmarket", "Oak Park",
                       "Pascoe Vale", "Roxburgh Park", "Strathmore")  
        ~ "Craigieburn",
        
        Station %in% c("Cranbourne", "Lynbrook", "Merinda Park")      
        ~ "Cranbourne",
        
        Station %in% c("Aspendale", "Bentleigh", "Bonbeach", "Carrum",
                       "Chelsea", "Cheltenham", "Edithvale", "Glenhuntly", 
                       "Highett", "Kananook", "McKinnon", "Mentone", 
                       "Moorabbin", "Mordialloc", "Ormond", "Parkdale", 
                       "Patterson", "Seaford", "Southland" )          
        ~ "Frankston",
        
        Station %in% c("Darling", "East Malvern", "Gardiner", "Glen Iris", 
                       "Glen Waverley", "Heyington", "Holmesglen", "Jordanville", 
                       "Kooyong", "Mount Waverley",  "Syndal", "Tooronga")                                   
        ~ "Glen Waverley",
        
        Station %in% c("Alphington", "Darebin", "Dennis", "Diamond Creek",
                       "Eaglemont", "Eltham", "Fairfield", "Greensborough", 
                       "Heidelberg", "Hurstbridge", "Ivanhoe", "Macleod", 
                       "Montmorency", "Rosanna", "Watsonia", "Wattle Glen", 
                       "Westgarth" )                                 
        ~ "Hurstbridge",
        
        Station %in% c("Croydon", "Lilydale", "Mooroolbark", "Ringwood East")
        ~ "Lilydale",
        
        Station %in% c("Bell", "Croxton", "Epping", "Hawkstowe",
                       "Keon Park", "Lalor", "Mernda", "Merri",
                       "Middle Gorge", "Northcote", "Preston", "Regent", 
                       "Reservoir", "Rushall", "Ruthven", "South Morang", 
                       "Thomastown", "Thornbury")                    
        ~ "Mernda",
        
        Station %in% c("Beaconsfield", "Berwick", "Cardinia Road", "Hallam",
                       "Narre Warren", "Officer", "Pakenham")        
        ~ "Pakenham",
        
        Station %in% c("Balaclava", "Brighton Beach", "Elsternwick", "Gardenvale",
                       "Hampton", "Middle Brighton", "North Brighton", "Prahran",
                       "Ripponlea", "Sandringham", "Windsor")        
        ~ "Sandringham",
        
        Station %in% c("Baxter", "Bittern", "Crib Point", "Hastings",
                       "Leawarra", "Morradoo", "Somerville", "Stony Point",
                       "Tyabb")                                      
        ~ "Stony Point",
        
        Station %in% c("Albion", "Diggers Rest", "Ginifer", "Keilor Plains", 
                       "Middle Footscray", "St Albans", "Sunbury", "Sunshine",
                       "Tottenham", "Watergardens", "West Footscray") 
        ~ "Sunbury",
        
        Station %in% c("Anstey", "Batman", "Brunswick", "Coburg", 
                       "Flemington Bridge", "Fawkner", "Gowrie", "Jewell", 
                       "Macaulay", "Merlynston", "Moreland", "Royal Park", 
                       "Upfield")                                     
        ~ "Upfield",
        
        Station %in% c("Aircraft", "Altona", "Hoppers Crossing", "Laverton",
                       "Seaholme", "Westona", "Williams Landing", "Werribee")
        
        ~ "Werribee",
        
        Station %in% c("North Williamstown", "Williamstown", "Williamstown Beach")
        ~ "Williamstown",
        
        # multiple lines
        Station %in% c("Auburn", "Camberwell", "Glenferrie", "Hawthorn")
        ~ "Alamein, Belgrave, Lilydale",
        
        Station %in% c("Flagstaff", "Flinders Street", "Melbourne Central", 
                       "Parliament", "Southern Cross")
        ~ "Alamein, Belgrave, Craigieburn, Cranbourne, Frankston, Glen Waverley, Hurstbridge, Lilydale, Mernda, Pakenham, Sandringham, Sunbury, Upfield, Werribee, Williamstown",
        
        Station %in% c("Richmond")
        ~ "Alamein, Belgrave, Cranbourne, Frankston, Glen Waverley, Lilydale, Pakenham, Sandringham",
        
        Station %in% c("Burnley", "East Richmond")                        
        ~ "Alamein, Belgrave, Glen Waverley, Lilydale",
        
        Station %in% c("Blackburn", "Box Hill", "Canterbury", "Chatham",
                       "East Camberwell", "Heatherdale", "Laburnum", "Mitcham",
                       "Mont Albert", "Nunawading", "Ringwood", "Surrey Hills")
        ~ "Belgrave, Lilydale",
        
        Station %in% c("North Melbourne")   
        ~ "Craigieburn, Sunbury, Upfield, Werribee, Williamstown",
        
        Station %in% c("Carnegie", "Clayton", "Dandenong", "Hughesdale",
                       "Huntingdale", "Murrumbeena", "Oakleigh", "Noble Park",
                       "Sandown Park", "Springvale", "Westall", "Yarraman")
        ~ "Cranbourne, Pakenham",
        
        Station %in% c("Armadale", "Caulfield", "Hawksburn", "Malvern",
                       "Toorak")
        ~ "Cranbourne, Frankston, Pakenham",
        
        Station %in% c("South Yarra")
        ~ "Cranbourne, Frankston, Pakenham, Sandringham",
        
        Station %in% c("Frankston")                      
        ~ "Frankston, Stony Point",
        
        Station %in% c("Clifton Hill", "Collingwood", "Jolimont", "North Richmond",
                       "Victoria Park", "West Richmond") 
        ~ "Hurstbridge, Mernda",
        
        Station %in% c("Footscray", "South Kensington")
        ~ "Sunbury, Werribee, Williamstown",
        
        Station %in% c("Newport", "Seddon", "Spotswood", "Yarraville")
        ~ "Werribee, Williamstown",
        
        TRUE ~ "TBA"  # for checking that all stations allocated - should be none
      ))
    
  )
}

