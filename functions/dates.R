# helper function to convert 'dates' in format 'Jul_2003' from text to date
to_date <- function(txt.date) {
  return(as.Date(paste0("01", txt.date), format = "%d%b_%Y"))
}
