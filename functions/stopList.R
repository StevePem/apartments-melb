# function to convert stop column in apartment dataframe into vector of unique elements

stopList <- function(stop.column) {
  
  # convert column to a single string
  stops <- toString(paste0(stop.column, 
                           collapse = ','))  %>%
    
    # remove any NA
    gsub("NA", "", .) %>%
    # 
    # replace any 2 or more commas (occurs when a row is empty) with single comma
    gsub(",{2,}", ",", .) %>%
    
    # omit white space after commas (occurs when a row contains more than one stop)
    gsub(", ", ",", .) %>%  
    
    # split at commas into a list of individual strings
    strsplit(., ",") %>%
    
    # unlist, unique and sort
    unlist() %>%
    unique() %>% 
    sort() %>%
    
    # remove any empty strings
   .[nzchar(.)]
  
  return(stops)
}
