# function to extract value changes for working paper appendix (based on dashboard.Rmd plot) 

appendixTable <- function(location_i, filtered_data) {
  
  # location_i = location_i
  # filtered_data = data.tt
  
  # percentage change in 'apartments' from first to last year
  apt.start <- filtered_data %>%
    filter(Year == min(Year)) %>%
    pull(Apartments)
  apt.end <- filtered_data %>%
    filter(Year == max(Year)) %>%
    pull(Apartments)
  apt.change <- round((apt.end - apt.start) / apt.start * 100, 1)
  
  
  # percentage change in 'population' from first to last year
  pop.start <- filtered_data %>%
    filter(Year == min(Year)) %>%
    pull(Population)
  pop.end <- filtered_data %>%
    filter(Year == max(Year)) %>%
    pull(Population)
  pop.change <- round((pop.end - pop.start) / pop.start * 100, 1)
  
  
  # percentage change in 'service' from first to last year, excluding zero years
  serv.start <- filtered_data %>%
    filter(service > 0) %>%
    filter(Year == min(Year)) %>%
    pull(service)
  # if all years zero, then set at zero
  if (length(serv.start) == 0) {
    serv.start <- 0
  }
  serv.end <- filtered_data %>%
    filter(Year == max(Year)) %>%
    pull(service)
  serv.change <- round((serv.end - serv.start) / serv.start * 100, 1)
  
  
  # percentage change in 'service capadj' from first to last year, excluding zero years
  serv_capadj.start <- filtered_data %>%
    filter(service.capadj > 0) %>%
    filter(Year == min(Year)) %>%
    pull(service.capadj)
  # if all years zero, then set at zero
  if (length(serv_capadj.start) == 0) {
    serv_capadj.start <- 0
  }
  serv_capadj.end <- filtered_data %>%
    filter(Year == max(Year)) %>%
    pull(service.capadj)
  serv_capadj.change <- round((serv_capadj.end - serv_capadj.start) / serv_capadj.start * 100, 1)
  
  # combine outputs into dataframe
  output <- data.frame(rbind(apt.change,
                             pop.change,
                             serv.change,
                             serv_capadj.change))
  colnames(output) <- location_i
  
  return(output)
  
}
