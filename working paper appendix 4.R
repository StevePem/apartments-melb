# script to generate comparison outputs for tram routes 16, [6 and 58],
# based on route km instead of service km, for working paper

# Background:

# Route 16:
# - From 17/10/04, route 16 (Melbourne University to St Kilda Beach) merged with
#   route 69 (St Kilda Beach to Kew) to form route 16 (Melbourne University to Kew)

# Routes 6 and 58:
# - From 17/10/04, route 8 (Melbourne University to Toorak) merged with route 22
#   (Moreland to Arts Precinct) to form route 8 (Moreland to Toorak)
# - From 1/5/17, route 8 split at Domain Interchange into sections of route 6 
#   (Moreland to Glen Iris) and route 58 (West Coburg to Toorak) 
# - From 1/7/17, route 58 altered to run via Toorak Rd instead of Domain Rd
 

# Note - step 1 begins with running parts of analysis.R, and subsequent
# steps assume that libraries and functions have been loaded from there;
# if starting elsewhere, run sections 1.1 and 1.2 of analysis.R to load them


# 1 Monthly totals for routes ----
# -----------------------------------------------------------------------------#
# Using analysis.R, create monthly totals for routes 16, [6 and 58] as follows

# Step through sections 1 and 2 of analysis.R (though not all these inputs are needed)

# Load selection areas and geog
corridors <- st_read("../GIS/corridors.sqlite")
cordon <-  read_zipped_GIS(zipfile = "../Data/LGA.zip",
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN") %>%
  filter(LGA_NAME == "MELBOURNE") %>%
  st_buffer(., 120)

selection.areas <- corridors
geog <- "corridors_own_services_with_apts"


# At section 3, set i=33 (for route 16), i=xx (for route 6 north) or
# i=xx (for route 58 south)  # CONSIDER ROUTES 6/58 FURTHER
i <- 33  # route 16
i <- 42  # route 6 north
i <- 40  # route 58 south

# Step through section 3 up to and including section 3.3.2, and save outputs

if (i == 33) {
  write.csv(tram.monthly.volumes, 
            "../Appendix/appendix 4/tram monthly volumes route 16.csv", row.names = FALSE)
} else if (i == 42) {
  write.csv(tram.monthly.volumes, 
            "../Appendix/appendix 4/tram monthly volumes route 6 north.csv", row.names = FALSE)
} else if (i == 40) {
  write.csv(tram.monthly.volumes, 
            "../Appendix/appendix 4/tram monthly volumes route 58 south.csv", row.names = FALSE)
  
}


# 2 Convert service volumes to distance volumes ----
# -----------------------------------------------------------------------------#
## 2.1 Route lengths (in km), data and setup ----
## -------------------------------------#
# read in digitised routes (manually traced from PPTN)
app.4.routes <- st_read("../Appendix/appendix 4/app 4 routes.sqlite",
                        layer = "app 4 routes") %>%
  # add distance in km
  mutate(length = as.numeric(st_length(.)) / 1000)

route.16.old.length <- app.4.routes %>% filter(route == "16 old") %>% 
  .$length  # Melb Uni to St Kilda
route.69.length <- app.4.routes %>% filter(route == "69") %>% 
  .$length  # St Kilda to Kew
route.16.new.length <- app.4.routes %>% filter(route == "16 new") %>% 
  .$length  # Melb Uni to Kew
route.8.old.length <- app.4.routes %>% filter(route == "8 old") %>% 
  .$length  # Melb Uni to Toorak
route.22.length <- app.4.routes %>% filter(route == "22") %>% 
  .$length  # Moreland to Arts Precinct
route.8.new.north.length <- app.4.routes %>% filter(route == "8 new north") %>% 
  .$length # Moreland to Domain Interchange
route.8.new.south.length <-  app.4.routes %>% filter(route == "8 new south") %>% 
  .$length # Domain Interchange to Toorak
route.6.north.length <- app.4.routes %>% filter(route == "6 north") %>% 
  .$length  # Moreland to Domain Interchange
route.58.south.domain.rd.length <- app.4.routes %>% filter(route == "58 south domain rd") %>% 
  .$length # Domain Interchange to Toorak via Domain Rd
route.58.south.toorak.rd.length <- app.4.routes %>% filter(route == "58 south toorak rd") %>% 
  .$length # Domain Interchange to Toorak via Toorak Rd

# load data as for dashboard
all.data <- readRDS("./dashboard/data.rds")

# setup to print via orca (see section 2 of working paper appendix 1.R
# for more detail on preliminary steps to using orca)
library(orca)
Sys.setenv(PATH = paste0("C:/Users/steve/AppData/Local/Programs/orca/", ";", Sys.getenv("PATH")))
px_cm <- 37.8  # conversion factor for px to cm (assumes device resolution of 96 PPI; vary if different


## 2.2 Function for annual totals (from section 3.3 of analysis.R) ----
## -------------------------------------#
tramAnnualTotals <- function(tram.monthly.volumes) {
  tram.annual.totals <- colSums(tram.monthly.volumes %>%
                                  dplyr::select(-route)) %>%
    # this creates a 'named numeric vector' of the totals; convert to dataframe
    data.frame() %>%
    # convert row names into a column and set the names
    tibble::rownames_to_column() %>%
    setNames(., c("rowname", "volume")) %>%
    # split the 'rownames' column at "_" (ignore the warning message, which
    # arises because some columns intentionally don't have 'capadj')
    separate(col = "rowname", into = c("month", "year", "capadj"), sep = "_") %>%
    
    # add a fin_year column
    rowwise() %>%
    mutate(fin_year = if_else(month %in% c("Jul", "Aug", "Sep", 
                                           "Oct", "Nov", "Dec"),
                              as.numeric(year) + 1,
                              as.numeric(year))) %>%
    
    # group by fin year and sum
    group_by(fin_year, capadj) %>%
    summarise(annual.total = sum(volume)) %>%
    
    # pivot wider, to make separate volume and capadj columns
    pivot_wider(names_from = capadj, values_from = annual.total) %>%
    # rename and reorder columns
    dplyr::select(Year = fin_year, service = "NA", service.capadj = capadj)
  
  # output is a table with 3 columns: Year, service, service.capadj (corresponding to
  # fin_year, tram.volume, and tram.volume.capadj in section 3.3 of analysis.R)
  
  return(tram.annual.totals)
  
}


## 2.3 Route 16 ----
## -------------------------------------#
# update route 16 for distances
route.16.services <- read.csv("../Appendix/appendix 4/tram monthly volumes route 16.csv")

# start with copy of services, then multiply months by relevant distances (where not zero)
route.16.distances <- route.16.services

# Jul 2003 to Sep 2004, route 16 (row 1) and route 69 (row 2)
route.16.distances[1, 2:31] <- route.16.distances[1, 2:31] * route.16.old.length
route.16.distances[2, 2:31] <- route.16.distances[2, 2:31] * route.69.length

# Oct 2004, route 16 (row 1) apportioned, and route 69 (row 2)
route.16.distances[1, 32:33] <- 
  (route.16.distances[1, 32:33] * route.16.old.length * 16/31) +
  (route.16.distances[1, 32:33] * route.16.new.length * 15/31)
route.16.distances[2, 32:33] <- route.16.distances[2, 32:33] * route.69.length

# Nov 2004 to Jun 2022, route 16 (row 1) only
route.16.distances[1, 34:457] <- route.16.distances[1, 34:457] * route.16.new.length

# get annual totals
route.16.annual.totals <- tramAnnualTotals(route.16.distances)

# get other data for route 16, and substitute distances for tram
data.16 <- all.data %>%
  filter(location == "Route 16") %>%
  # substitue distances
  dplyr::select(-Tram, -Tram.capadj) %>%
  left_join(route.16.annual.totals, by = "Year")

# create and save plot
plot.16 <- appendixPlot(data.16)
suppressWarnings(orca(plot.16, "../Appendix/appendix 4/Route 16.svg",
                      width = 14.5 * px_cm, height = 14.5 * px_cm))


## 2.4 Route 6 north ----
## -------------------------------------#
# update route 6 north for distances
route.6n.services <- read.csv("../Appendix/appendix 4/tram monthly volumes route 6 north.csv")

# start with copy of services, then multiply months by relevant distances (where not zero)
route.6n.distances <- route.6n.services

# Jul 2003 to Sep 2004, route 22 (row 1) only
route.6n.distances[1, 2:31] <- route.6n.distances[1, 2:31] * route.22.length

# Oct 2004, route 22 (row 1) and route 8 (row 3)
route.6n.distances[1, 32:33] <- route.6n.distances[1, 32:33] * route.22.length 
route.6n.distances[3, 32:33] <- route.6n.distances[3, 32:33] * route.8.new.north.length

# Nov 2004 to Apr 2017, route 8 (row 3) only
route.6n.distances[3, 34:333] <- route.6n.distances[3, 34:333] * route.8.new.north.length

# May 2017 to Jun 2022, route 6 (row 2) only
route.6n.distances[2, 334:457] <- route.6n.distances[2, 334:457] * route.6.north.length

# get annual totals
route.6n.annual.totals <- tramAnnualTotals(route.6n.distances)

# get other data for route 16, and substitute distances for tram
data.6n <- all.data %>%
  filter(location == "Route 6 north") %>%
  # substitue distances
  dplyr::select(-Tram, -Tram.capadj) %>%
  left_join(route.6n.annual.totals, by = "Year")

# create and save plot
plot.6n <- appendixPlot(data.6n)
suppressWarnings(orca(plot.6n, "../Appendix/appendix 4/Route 6 north.svg",
                      width = 14.5 * px_cm, height = 14.5 * px_cm))


## 2.5 Route 58 south ----
## -------------------------------------#
# update route 58 south for distances
route.58s.services <- read.csv("../Appendix/appendix 4/tram monthly volumes route 58 south.csv")

# start with copy of services, then multiply months by relevant distances (where not zero)
route.58s.distances <- route.58s.services

# Jul 2003 to Sep 2004, route 8 (row 2) only
route.58s.distances[2, 2:31] <- route.58s.distances[2, 2:31] * route.8.old.length

# Oct 2004, route 8 (row 2) apportioned
route.58s.distances[2, 32:33] <- 
  (route.58s.distances[2, 32:33] * route.8.old.length * 16/31) +
  (route.58s.distances[2, 32:33] * route.8.new.south.length * 15/31)

# Nov 2004 to Apr 2017, route 8 (row 2) only
route.58s.distances[2, 34:333] <- route.58s.distances[2, 34:333] * route.8.new.south.length

# May 2017 to Jun 2017, route 58 (row 1) only
route.58s.distances[1, 334:337] <- route.58s.distances[1, 334:337] * route.58.south.domain.rd.length

# Jul 2017 to Jun 2022, route 58 (row 1) only
route.58s.distances[1, 338:457] <- route.58s.distances[1, 338:457] * route.58.south.toorak.rd.length

# get annual totals
route.58s.annual.totals <- tramAnnualTotals(route.58s.distances)

# get other data for route 16, and substitute distances for tram
data.58s <- all.data %>%
  filter(location == "Route 58 south") %>%
  # substitue distances
  dplyr::select(-Tram, -Tram.capadj) %>%
  left_join(route.58s.annual.totals, by = "Year")

# create and save plot
plot.58s <- appendixPlot(data.58s)
suppressWarnings(orca(plot.58s, "../Appendix/appendix 4/Route 58 south.svg",
                      width = 14.5 * px_cm, height = 14.5 * px_cm))

