# function to modify DoT's tram service volume figures to account for routes
# that DoT has bundled together under a single parent route

# see tram_route_changes.xlsx for details of the determination of the adjustments

tramVoltableSplitRoutes <- function(tram.voltable, adj.factors) {
  # adjustments - (1) create separate table for the secondary route, with its
  # volume adjusted; (2) adjust the volume of the primary route; (3) bind the 
  # separate table into the main table
  
  # route 3 operates as 3/3a from 31/1/09
  ## separate table for route 3a, with its adjusted volume 
  route.3a <- tram.voltable %>%
    filter(Route == "3" & date >= to_date("Jan_2009")) %>%
    mutate(Route = "3a") %>%
    mutate(sched.vol = case_when(
      date == to_date("Jan_2009") ~ 
        sched.vol * adj.factors[adj.factors$Route == "3a", "Factor"][[1]] * 1/31,
      date > to_date("Jan_2009") ~ 
        sched.vol * adj.factors[adj.factors$Route == "3a", "Factor"][[1]]
    ))
  
  tram.voltable <- tram.voltable %>%
    ## adjusted volume for route 3
    mutate(sched.vol = case_when(
      Route == "3" & date == to_date("Jan_2009") ~ 
        (sched.vol * 30/31) + 
        (sched.vol * adj.factors[adj.factors$Route == "3", "Factor"][[1]] * 1/31),
      Route == "3" & date > to_date("Jan_2009") ~
        sched.vol * adj.factors[adj.factors$Route == "3", "Factor"][[1]],
      # route 3 before Jan_2009, and all other routes
      TRUE ~ sched.vol
    )) %>%
    ## bind in the separate route 3a table
    rbind(., route.3a)
  
  
  # route 5 operates as 5/5s at all times
  ## separate table for route 5s, with its adjusted volume 
  route.5s <- tram.voltable %>%
    filter(Route == "5") %>%
    mutate(Route = "5s") %>%
    mutate(sched.vol = 
             sched.vol * adj.factors[adj.factors$Route == "5s", "Factor"][[1]])
  
  tram.voltable <- tram.voltable %>%
    ## adjusted volume for route 5
    mutate(sched.vol = case_when(
      Route == "5" ~
        sched.vol * adj.factors[adj.factors$Route == "5", "Factor"][[1]],
      # all other routes
      TRUE ~ sched.vol
    )) %>%
    ## bind in the separate route 5s table
    rbind(., route.5s)
  
  
  # route 55 operated as 55/68 before 07/08/05 
  ## separate table for route 68, with its adjusted volume 
  route.68 <- tram.voltable %>%
    filter(Route == "55" & date <= to_date("Aug_2005")) %>%
    mutate(Route = "68") %>%
    mutate(sched.vol = case_when(
      date < to_date("Aug_2005") ~ 
        sched.vol * adj.factors[adj.factors$Route == "68", "Factor"][[1]],
      date == to_date("Aug_2005") ~ 
        sched.vol * adj.factors[adj.factors$Route == "68", "Factor"][[1]] * 6/31
    ))
  
  tram.voltable <- tram.voltable %>%
    ## adjusted volume for route 55
    mutate(sched.vol = case_when(
      Route == "55" & date < to_date("Aug_2005") ~
        sched.vol * adj.factors[adj.factors$Route == "55", "Factor"][[1]],
      Route == "55" & date == to_date("Aug_2005") ~ 
        (sched.vol * adj.factors[adj.factors$Route == "55", "Factor"][[1]] * 6/31) +
        (sched.vol * 25/31),
      # route 55 after Aug_2005, and all other routes
      TRUE ~ sched.vol
    )) %>%
    ## bind in the separate route 68 table
    rbind(., route.68)
  
  # route 78 operated as 78/79 before 27/07/14, with one pattern before
  # 20/7/09 and a different pattern from that date
  ## separate table for route 68, with its adjusted volume 
  route.79 <- tram.voltable %>%
    filter(Route == "78" & date <= to_date("Jul_2014")) %>%
    mutate(Route = "79") %>%
    mutate(sched.vol = case_when(
      date < to_date("Jul_2009") ~ 
        sched.vol * adj.factors[adj.factors$Route == "79", "Factor"][[1]],
      date == to_date("Jul_2009") ~
        (sched.vol * adj.factors[adj.factors$Route == "79", "Factor"][[1]] * 19/31) +
        (sched.vol * adj.factors[adj.factors$Route == "79", "Factor2"][[1]] * 12/31),
      date > to_date("Jul_2009") & date < to_date("Jul_2014") ~
        sched.vol * adj.factors[adj.factors$Route == "79", "Factor2"][[1]],
      date == to_date("Jul_2014") ~
        sched.vol * adj.factors[adj.factors$Route == "79", "Factor2"][[1]] * 26/31
    ))
  
  tram.voltable <- tram.voltable %>%
    ## adjusted volume for route 78
    mutate(sched.vol = case_when(
      Route == "78" & date < to_date("Jul_2009") ~ 
        sched.vol * adj.factors[adj.factors$Route == "78", "Factor"][[1]],
      Route == "78" & date == to_date("Jul_2009") ~
        (sched.vol * adj.factors[adj.factors$Route == "78", "Factor"][[1]] * 19/31) +
        (sched.vol * adj.factors[adj.factors$Route == "78", "Factor2"][[1]] * 12/31),
      Route == "78" & date > to_date("Jul_2009") & date < to_date("Jul_2014") ~
        sched.vol * adj.factors[adj.factors$Route == "78", "Factor2"][[1]],
      Route == "78" & date == to_date("Jul_2014") ~
        (sched.vol * adj.factors[adj.factors$Route == "78", "Factor2"][[1]] * 26/31) +
        (sched.vol * 5/31),
      # route 78 after Jul_2014, and all other routes
      TRUE ~ sched.vol
    )) %>%
    ## bind in the separate route 79 table
    rbind(., route.79)
  
  
  # 'city shuttle' comprises routes 30, 31 & 95 before 20/9/09, then routes 30, 71
  # & 95 before 24/10/10, then routes 30, 31 & 95 before 27/7/14, then route 30
  # only; during multi-route operation, assumed to be one/third each
  
  # separate table for route 31, with its adjusted volume
  route.31 <- tram.voltable %>%
    filter(Route == "City Shuttle" & (date <= to_date("Sep_2009") |
                                        (date >= to_date("Oct_2010") & 
                                           date <= to_date("Jul_2014")))) %>%
    mutate(Route = "31") %>%
    mutate(sched.vol = case_when(
      date < to_date("Sep_2009") ~ sched.vol / 3,
      date == to_date("Sep_2009") ~ (sched.vol / 3) * 19/30,
      date == to_date("Oct_2010") ~ (sched.vol / 3) * 8/31,
      date > to_date("Oct_2010") & date < to_date("Jul_2014") ~ sched.vol / 3,
      date == to_date("Jul_2014") ~ (sched.vol / 3) * 26/31
    ))
  
  # separate table for route 71, with its adjusted volume
  route.71 <- tram.voltable %>%
    filter(Route == "City Shuttle" & (date >= to_date("Sep_2009") &
                                        date <= to_date("Oct_2010"))) %>%
    mutate(Route = "71") %>%
    mutate(sched.vol = case_when(
      date == to_date("Sep_2009") ~ (sched.vol / 3) * 11/30,
      date > to_date("Sep_2009") & date < to_date("Oct_2010") ~ sched.vol / 3,
      date == to_date("Oct_2010") ~ (sched.vol / 3) * 23/31
    ))
  
  # separate table for route 95, with its adjusted volume
  route.95 <- tram.voltable %>%
    filter(Route == "City Shuttle" & date <= to_date("Jul_2014")) %>%
    mutate(Route = "95") %>%
    mutate(sched.vol = case_when(
      date < to_date("Jul_2014") ~ sched.vol / 3,
      date == to_date("Jul_2014") ~ (sched.vol / 3) * 26/31
    ))
  
  tram.voltable <- tram.voltable %>%
    ## change 'City Shuttle' to 30
    mutate(Route = case_when(
      Route == "City Shuttle" ~ "30",
      TRUE ~ Route
    )) %>%
    ## adjusted volume for route 30
    mutate(sched.vol = case_when(
      Route == "30" & date < to_date("Jul_2014") ~ 
        sched.vol / 3,
      Route == "30" & date == to_date("Jul_2014") ~ 
        ((sched.vol / 3) * 26/31) + (sched.vol * 5/31),
      # route 30 after Jul_2014, and all other routes
      TRUE ~ sched.vol
    )) %>%
    ## bind in the separate route 31, 71 & 95 tables
    rbind(., route.31, route.71, route.95)
  
}